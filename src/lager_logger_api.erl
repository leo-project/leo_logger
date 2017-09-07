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

    ok = application:set_env(lager, handlers,
                             [{lager_file_backend, [{file, ?LOG_FILE_NAME_INFO}, {level, none},
                                                    {size, 10485760}, {date, "$H0"}, {count, 100},
                                                    {formatter, lager_leofs_formatter},
                                                    {formatter_config, ["[", sev, "]\t", atom_to_list(node()), "\t", leodate, "\t", leotime, "\t", {module, "null"}, ":", {function, "null"}, "\t", {line, "0"}, "\t", message, "\n"]},
                                                    {rotator, leo_logger_rotator}
                                                   ]},
                              {lager_file_backend, [{file, ?LOG_FILE_NAME_ERROR}, {level, none},
                                                    {size, 10485760}, {date, "$H0"}, {count, 100},
                                                    {formatter, lager_leofs_formatter},
                                                    {formatter_config, ["[", sev, "]\t", atom_to_list(node()), "\t", leodate, "\t", leotime, "\t", {module, "null"}, ":", {function, "null"}, "\t", {line, "0"}, "\t", message, "\n"]},
                                                    {rotator, leo_logger_rotator}
                                                   ]}
                             ]),
    ok = application:set_env(lager, error_logger_hwm, 500),

%%    ok = application:set_env(lager, extra_sinks,
%%                             [{access_lager_event,
%%                               [{handlers,
%%                                 [{lager_file_backend, [{file, ?LOG_FILENAME_ACCESS}, {level, none},
%%                                                        {size, 10485760}, {date, "$D0"}, {count, 100},
%%                                                        {formatter, lager_default_formatter},
%%                                                        {formatter_config, [message, "\n"]}
%%                                                       ]}]
%%                                },
%%                                {async_threshold, 500},
%%                                {async_threshold_window, 50}]
%%                              }]),

    {ok, Handlers} = ?log_handlers(Level),

    lager:start(),
    lists:foreach(fun({File, FLevel}) ->
                          lager:set_loglevel(lager_file_backend, File, FLevel)
                  end, Handlers),

%%        case application:get_env(leo_gateway, is_enable_access_log) of
%%            {ok, true} ->
%%                ok = lager:set_loglevel(access_lager_event, lager_file_backend, "access.log", info);
%%            _ ->
%%                void
%%        end,

    ok.

-spec(new(LogGroup, LogId, RootPath, LogFileName) ->
             ok when LogGroup::atom(),
                     LogId::atom(),
                     RootPath::string(),
                     LogFileName::string()).
new(LogGroup, LogId, RootPath, LogFileName) ->
    leo_logger_client_base:new(LogGroup, LogId, RootPath, LogFileName).

%% @doc Append a message to a file
-spec(append(LogInfo) ->
             ok when LogInfo::{atom(), #message_log{}}).
append(LogInfo) ->
    leo_logger_client_base:append(LogInfo).

%% @doc Update the log level of the info/error logger
-spec(update_log_level(Level) ->
             ok | {error, any()} when Level::log_level()).
update_log_level(Val) ->
    leo_logger_client_message:update_log_level(Val).


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
    application:stop(lager),
    leo_logger_sup:stop().
