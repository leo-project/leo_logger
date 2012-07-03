%%======================================================================
%%
%% Leo Logger
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
%% Leo Logger - AMQP Appender
%% @doc
%% @end
%%======================================================================
-module(leo_logger_appender_amqp).

-author('Yosuke Hara').
-vsn('0.9.0').

-behaviour(leo_logger_behavior).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, append/2, rotate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize this logger
%%
-spec(init(atom(), list(), list()) ->
             {ok, #logger_state{}}).
init(Appender, Callback, Props) ->
    %% ?debugVal({Appender, Callback, Props}),
    {ok, #logger_state{appender_type = Appender,
                appender_mod  = ?appender_mod(Appender),
                props         = Props,
                callback      = Callback}}.


%% @doc Append a message to a file
%%
-spec(append(binary(), #logger_state{}) ->
             ok).
append(_FormattedMsg, _State) ->
    %% ?debugVal({FormattedMsg, State}),
    ok.


%% @doc
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(_Hours, #logger_state{props = _Props} = State) ->
    %% ?debugVal({Hours, Props, State}),
    {ok, State}.

