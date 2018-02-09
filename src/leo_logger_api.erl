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
-module(leo_logger_api).

-include("leo_logger.hrl").

-author('Wilson Li').

-export([new/2, new/3, new/4,
         append/1,
         update_log_level/1,
         debug/1, info/1, warn/1, error/1, fatal/1,
         reset_hwm/0,
         stop/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create loggers for message logs
%%
-spec(new(RootPath, Level) ->
             ok when RootPath::string(),
                     Level::integer()).
new(RootPath, Level) ->
    leo_logger_client_message:new(RootPath, Level).

-spec(new(RootPath, Level, Loggers) ->
             ok when RootPath::string(),
                     Level::integer(),
                     Loggers::[{atom(), log_appender()}]).
new(RootPath, Level, Loggers) ->
    leo_logger_client_message:new(RootPath, Level, Loggers).

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


%% @doc Output kind of 'Debug log'
-spec(debug(Log) ->
             ok when Log::#message_log{}).
debug(Log) ->
    leo_logger_client_message:debug(Log).


%% @doc Output kind of 'Information log'
-spec(info(Log) ->
             ok when Log::#message_log{}).
info(Log) ->
    leo_logger_client_message:info(Log).


%% @doc Output kind of 'Warning log'
-spec(warn(Log) ->
             ok when Log::#message_log{}).
warn(Log) ->
    leo_logger_client_message:warn(Log).


%% @doc Output kind of 'Error log'
-spec(error(Log) ->
             ok when Log::#message_log{}).
error(Log) ->
    leo_logger_client_message:error(Log).


%% @doc Output kind of 'Fatal log'
-spec(fatal(Log) ->
             ok when Log::#message_log{}).
fatal(Log) ->
    leo_logger_client_message:fatal(Log).


%% @doc Stop Loggers
stop() ->
    leo_logger_sup:stop().

%% @doc Reset error_logger_hwm
%%      It should be called once the startup phase finished.
reset_hwm() ->
    % nop for the original leo_logger
    ok.
