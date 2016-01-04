%%======================================================================
%%
%% Leo Logger
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc The logger client for handling messages
%% @reference https://github.com/leo-project/leo_logger/blob/master/src/leo_logger_client_message.erl
%% @end
%%======================================================================
-module(leo_logger_client_message).

-author('Yosuke Hara').

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([new/2, new/3,
         update_log_level/1,
         debug/1, info/1, warn/1, error/1, fatal/1,
         format/2]).

-define(LOG_FILE_NAME_INFO, "info").
-define(LOG_FILE_NAME_ERROR, "error").
-define(LOG_GROUP_INFO, 'log_group_message_info').
-define(LOG_GROUP_ERROR, 'log_group_message_error').
-define(MAX_MSG_BODY_LEN, 4096).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create loggers for message logs
%%
-spec(new(RootPath, Level) ->
             ok when RootPath::string(),
                     Level::integer()).
new(RootPath, Level) ->
    new(RootPath, Level, [{?LOG_ID_FILE_INFO, ?LOG_APPENDER_FILE},
                          {?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}]).

-spec(new(RootPath, Level, Loggers) ->
             ok when RootPath::string(),
                     Level::integer(),
                     Loggers::[{atom(), log_appender()}]).
new(RootPath, Level, Loggers) ->
    %% change error-logger
    case gen_event:which_handlers(error_logger) of
        [leo_logger_error_logger_h] ->
            void;
        _ ->
            ok = gen_event:add_sup_handler(error_logger,
                                           leo_logger_error_logger_h, []),
            _ = [begin
                     error_logger:delete_report_handler(X),
                     X
                 end || X <- gen_event:which_handlers(error_logger)
                            -- [leo_logger_error_logger_h]]
    end,

    %% create loggers
    lists:foreach(fun({Id, Appender}) ->
                          case Appender of
                              ?LOG_APPENDER_FILE when Id == ?LOG_ID_FILE_INFO ->
                                  ok = leo_logger_util:new(Id, Appender, ?MODULE,
                                                           RootPath, ?LOG_FILE_NAME_INFO, Level),
                                  ok = leo_logger_util:add_appender(?LOG_GROUP_INFO, Id);
                              ?LOG_APPENDER_FILE when Id == ?LOG_ID_FILE_ERROR ->
                                  ok = leo_logger_util:new(Id, Appender, ?MODULE,
                                                           RootPath, ?LOG_FILE_NAME_ERROR, Level),
                                  ok = leo_logger_util:add_appender(?LOG_GROUP_ERROR,Id);
                              _ ->
                                  ok = leo_logger_util:new(Id, Appender, ?MODULE),
                                  ok = leo_logger_util:add_appender(?LOG_GROUP_INFO, Id),
                                  ok = leo_logger_util:add_appender(?LOG_GROUP_ERROR,Id)
                          end
                  end, Loggers),
    ok.


%% @doc Update the log level of the info/error logger
-spec(update_log_level(Level) ->
             ok | {error, any()} when Level::log_level()).
update_log_level(Level) when Level /= ?LOG_LEVEL_DEBUG,
                             Level /= ?LOG_LEVEL_INFO,
                             Level /= ?LOG_LEVEL_WARN,
                             Level /= ?LOG_LEVEL_ERROR,
                             Level /= ?LOG_LEVEL_FATAL ->
    {error, badarg};
update_log_level(Level) ->
    case whereis(?LOG_ID_FILE_INFO) of
        undefined ->
            void;
        _ ->
            leo_logger_server:update_log_level(?LOG_ID_FILE_INFO, Level)
    end,
    case whereis(?LOG_ID_FILE_ERROR) of
        undefined ->
            void;
        _ ->
            leo_logger_server:update_log_level(?LOG_ID_FILE_ERROR, Level)
    end,
    ok.


%% @doc Output kind of 'Debug log'
-spec(debug(Log) ->
             ok when Log::#message_log{}).
debug(Log) ->
    append(?LOG_GROUP_INFO, Log, ?LOG_LEVEL_DEBUG).


%% @doc Output kind of 'Information log'
-spec(info(Log) ->
             ok when Log::#message_log{}).
info(Log) ->
    append(?LOG_GROUP_INFO, Log, ?LOG_LEVEL_INFO).


%% @doc Output kind of 'Warning log'
-spec(warn(Log) ->
             ok when Log::#message_log{}).
warn(Log) ->
    append(?LOG_GROUP_ERROR, Log, ?LOG_LEVEL_WARN).


%% @doc Output kind of 'Error log'
-spec(error(Log) ->
             ok when Log::#message_log{}).
error(Log) ->
    append(?LOG_GROUP_ERROR, Log, ?LOG_LEVEL_ERROR).


%% @doc Output kind of 'Fatal log'
-spec(fatal(Log) ->
             ok when Log::#message_log{}).
fatal(Log) ->
    append(?LOG_GROUP_ERROR, Log, ?LOG_LEVEL_FATAL).


%% @doc Format a log message
%%
-spec(format(Type, Log) ->
             string() when Type::atom(),
                           Log::#message_log{}).
format(_Type, Log) ->
    #message_log{format  = Format,
                 message = Message} = Log,
    FormattedMessage =
        case catch lager_format:format(
                     Format, Message, ?MAX_MSG_BODY_LEN) of
            {'EXIT', _} ->
                [];
            NewMessage ->
                NewMessage
        end,
    format_1(Log#message_log{message = FormattedMessage}).

%% @private
-spec(format_1(#message_log{}) ->
             string()).
format_1(#message_log{level    = Level,
                      module   = Module,
                      function = Function,
                      line     = Line,
                      message  = Message}) ->
    case catch lager_format:format("[~s]\t~s\t~s\t~w\t~s:~s\t~s\t~s\r\n",
                                   [log_level(Level),
                                    atom_to_list(node()),
                                    leo_date:date_format(),
                                    leo_date:unixtime(),
                                    Module, Function, integer_to_list(Line),
                                    Message], ?MAX_MSG_BODY_LEN) of
        {'EXIT', _Cause} ->
            [];
        Result ->
            Result
    end.


%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc append a log.
%% @private
-spec(append(GroupId, Log, Level) ->
             ok when GroupId::atom(),
                     Log::#message_log{},
                     Level::integer()).
append(GroupId, Log, Level) ->
    case catch ets:lookup(?ETS_LOGGER_GROUP, GroupId) of
        {'EXIT', Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "append/3"},
                                    {line, ?LINE}, {body, Cause}]),
            {error, Cause};
        [] ->
            {error, not_found};
        List ->
            Log_1 = Log#message_log{level = Level},
            lists:foreach(
              fun({_, AppenderId}) ->
                      leo_logger_server:append(
                        ?LOG_APPEND_SYNC, AppenderId, Log_1, Level)
              end, List)
    end.


%% @doc Set log-level
%% @private
-spec(log_level(Log) ->
             string() when Log::'undefined'|log_level()).
log_level(?LOG_LEVEL_DEBUG) -> "D";
log_level(?LOG_LEVEL_INFO)  -> "I";
log_level(?LOG_LEVEL_WARN)  -> "W";
log_level(?LOG_LEVEL_ERROR) -> "E";
log_level(?LOG_LEVEL_FATAL) -> "F";
log_level(_)                -> "_".
