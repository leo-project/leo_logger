%%======================================================================
%%
%% Leo Logger
%%
%% Copyright (c) 2012-2017 Rakuten, Inc.
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
%% ---------------------------------------------------------------------
%% Leo Logger - Application
%% @doc
%% @end
%%======================================================================
-module(lager_logger_api).

-include("leo_logger.hrl").

-author('Wilson Li').

-export([new/2, new/3, new/4,
         append/1,
         update_log_level/1,
         debug/1, info/1, warn/1, error/1, fatal/1,
         stop/0]).

-define(LOG_FILE_NAME_INFO, "info").
-define(LOG_FILE_NAME_ERROR, "error").
-define(LOG_FILE_NAME_ACCESS, "access").
-define(LOG_ID_TO_SINK_ETS, 'lager_logid_sink').

-define(log_handlers(_LogLvl),
        case _LogLvl of
            ?LOG_LEVEL_DEBUG ->
                {ok, [{?LOG_FILE_NAME_INFO, '<=info'},
                      {?LOG_FILE_NAME_ERROR, warning}]};
            ?LOG_LEVEL_INFO ->
                {ok, [{?LOG_FILE_NAME_INFO, '=info'},
                      {?LOG_FILE_NAME_ERROR, warning}]};
            ?LOG_LEVEL_WARN ->
                {ok, [{?LOG_FILE_NAME_INFO,  none},
                      {?LOG_FILE_NAME_ERROR, warning}]};
            ?LOG_LEVEL_ERROR ->
                {ok, [{?LOG_FILE_NAME_INFO,  none},
                      {?LOG_FILE_NAME_ERROR, error}]};
            ?LOG_LEVEL_FATAL ->
                {ok, [{?LOG_FILE_NAME_INFO,  none},
                      {?LOG_FILE_NAME_ERROR, critical}]};
            _ ->
                {error, badarg}
        end).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create loggers for message logs
%%
-spec(new(RootPath, Level) ->
             ok when RootPath::string(),
                     Level::integer()).
new(RootPath, Level) ->
    new(RootPath, Level, []).

-spec(new(RootPath, Level, Loggers) ->
             ok when RootPath::string(),
                     Level::integer(),
                     Loggers::[{atom(), log_appender()}]).
new(RootPath, Level, _Loggers) ->
    application:set_env(lager, log_root, RootPath),
    application:set_env(lager, crash_log, "crash.log"),
    application:set_env(lager, error_logger_hwm, 500),
    error_logger:info_msg("Setup Lager Logger API~n", []),

    application:set_env(lager, handlers,
                        [{lager_file_backend, [{file, ?LOG_FILE_NAME_INFO}, {level, none},
                                               {date, "$H0"},
                                               {formatter, lager_leofs_formatter},
                                               {formatter_config, ["[", sev, "]\t", atom_to_list(node()), "\t", leodate, "\t", leotime, "\t", {module, "null"}, ":", {function, "null"}, "\t", {line, "0"}, "\t", message, "\n"]},
                                               {rotator, leo_logger_rotator}
                                              ]},
                         {lager_file_backend, [{file, ?LOG_FILE_NAME_ERROR}, {level, none},
                                               {sync_on, critical}, {date, "$H0"},
                                               {formatter, lager_leofs_formatter},
                                               {formatter_config, ["[", sev, "]\t", atom_to_list(node()), "\t", leodate, "\t", leotime, "\t", {module, "null"}, ":", {function, "null"}, "\t", {line, "0"}, "\t", message, "\n"]},
                                               {rotator, leo_logger_rotator}
                                              ]}
                        ]),
    application:set_env(lager, async_threshold, undefined),
    application:set_env(lager, killer_hwm, 1000),

    lager:start(),

    {ok, Handlers} = ?log_handlers(Level),
    lists:foreach(fun({File, FLevel}) ->
                          lager:set_loglevel(lager_file_backend, File, FLevel)
                  end, Handlers),

    ets:new(?LOG_ID_TO_SINK_ETS,
            [named_table, bag, public, {read_concurrency, true}]),
    ok.

-spec(new(LogGroup, LogId, RootPath, LogFileName) ->
             ok when LogGroup::atom(),
                     LogId::atom(),
                     RootPath::string(),
                     LogFileName::string()).
new(LogGroup, LogId, _RootPath, LogFileName) ->
    lager_app:configure_sink(LogGroup,
                             [{handlers, [{lager_file_backend, [{file, LogFileName}, {level, info},
                                                                {size, 10485760}, {date, "$D0"},
                                                                {formatter, lager_default_formatter},
                                                                {formatter_config, [message, "\n"]},
                                                                {rotator, leo_logger_rotator}
                                                               ]}]},
                              {async_threshold, undefined},
                              {killer_hwm, 1000}]),
    ets:insert(?LOG_ID_TO_SINK_ETS, {LogId, {LogGroup, LogFileName}}),
    ok.

%% @doc Append a message to a file
-spec(append(LogInfo) ->
             ok when LogInfo::{atom(), #message_log{}}).
append({LogId, Log} = _LogInfo) ->
    case catch ets:lookup(?LOG_ID_TO_SINK_ETS, LogId) of
        {'EXIT', Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "append/1"},
                                    {line, ?LINE}, {body, Cause}]),
            {error, Cause};
        %% TODO: Check if lager can log to specific backend
        [{LogId, {LogGroup, _LogFileName}}] ->
            {Meta, Fmt, Msg} = convert_log(Log),
            lager:log(LogGroup, info, Meta, Fmt, Msg);
        _ ->
            {error, not_found}
    end.

%% @doc Update the log level of the info/error logger
-spec(update_log_level(Level) ->
             ok | {error, any()} when Level::log_level()).
update_log_level(Level) ->
    case ?log_handlers(Level) of
        {ok, Handlers} ->
            lists:foreach(fun({File, FLevel}) ->
                                  lager:set_loglevel(lager_file_backend, File, FLevel)
                          end, Handlers);
        Others ->
            Others
    end.

%% @doc Convert to Lager Metadata
-spec(convert_log(Log) ->
             {list(), list(), list()} when Log::#message_log{}).
convert_log(#message_log{module = Module,
                   function = Function,
                   line = Line,
                   format = Format,
                   message = Msg} = _Log) ->
    {[{pid, self()}, {module, Module},
      {function, Function}, {line, Line}],
     Format,
     Msg}.

%% @doc Output kind of 'Debug log'
-spec(debug(Log) ->
             ok when Log::#message_log{}).
debug(Log) ->
    {Meta, Fmt, Msg} = convert_log(Log),
    lager:log(debug, Meta, Fmt, Msg).

%% @doc Output kind of 'Information log'
-spec(info(Log) ->
             ok when Log::#message_log{}).
info(Log) ->
    {Meta, Fmt, Msg} = convert_log(Log),
    lager:log(info, Meta, Fmt, Msg).


%% @doc Output kind of 'Warning log'
-spec(warn(Log) ->
             ok when Log::#message_log{}).
warn(Log) ->
    {Meta, Fmt, Msg} = convert_log(Log),
    lager:log(warning, Meta, Fmt, Msg).


%% @doc Output kind of 'Error log'
-spec(error(Log) ->
             ok when Log::#message_log{}).
error(Log) ->
    {Meta, Fmt, Msg} = convert_log(Log),
    lager:log(error, Meta, Fmt, Msg).


%% @doc Output kind of 'Fatal log'
-spec(fatal(Log) ->
             ok when Log::#message_log{}).
fatal(Log) ->
    {Meta, Fmt, Msg} = convert_log(Log),
    lager:log(critical, Meta, Fmt, Msg).


%% @doc Stop Loggers
stop() ->
    ets:delete(?LOG_ID_TO_SINK_ETS),
    erase('__lager_file_backend_filenames'),
    application:stop(lager).

