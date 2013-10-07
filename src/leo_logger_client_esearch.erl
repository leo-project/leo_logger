%%======================================================================
%%
%% Leo Logger
%%
%% Copyright (c) 2012-2013 Rakuten, Inc.
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
%% Leo Logger - Client (access-log)
%% @doc
%% @end
%%======================================================================
-module(leo_logger_client_esearch).

-author('Yosuke Hara').

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([new/5, init/3,
         format/2, append/1, append/2, sync/1, rotate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create loggers for message logs
%%
-spec(new(atom(), atom(), string(), pos_integer(), pos_integer()) ->
             ok).
new(LogGroup, LogId, Host, Port, Timeout) ->
    ok = leo_logger_util:new(LogId, ?LOG_APPENDER_ESEARCH, [?MODULE, format],
                             [{?ESEARCH_PROP_HOST,    Host},
                              {?ESEARCH_PROP_PORT,    Port},
                              {?ESEARCH_PROP_TIMEOUT, Timeout}]),
    ok = leo_logger_util:add_appender(LogGroup, LogId),
    ok.


%% @doc Initialize
%%
-spec(init(atom(), list(atom()), list(any())) ->
             ok).
init(Appender, Callback, Props) ->
    leo_logger_appender_esearch:init(Appender, Callback, Props).


%% @doc Format a log message
%%
-spec(format(atom(), #message_log{}) ->
             ok).
format(Appender, Log) ->
    leo_logger_appender_esearch:format(Appender, Log).


%% @doc Append a message to a file
%%
-spec(append(any()) ->
             ok).
append({LogId, Log}) ->
    case whereis(LogId) of
        undefined ->
            ok;
        _Pid ->
            leo_logger_server:append(?LOG_APPEND_SYNC, LogId, Log, 0)
    end.

-spec(append(list(), #logger_state{}) ->
             ok).
append(FormattedMsg, State) ->
    leo_logger_appender_esearch:append(FormattedMsg, State).


%% @doc Sync a log file
%%
-spec(sync(atom|#logger_state{}) ->
             ok | {error, any()}).
sync(LogId) when is_atom(LogId) ->
    leo_logger_server:sync(LogId);
sync(State) when is_record(State, logger_state) ->
    leo_logger_appender_esearch:sync(State);
sync(_L) ->
    ok.

%% @doc Rotate a log
%%
-spec(rotate(pos_integer(), #logger_state{}) ->
             ok).
rotate(Hours, State) ->
    leo_logger_appender_esearch:rotate(Hours, State).

