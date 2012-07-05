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
%% Leo Logger - ZMQ Appender
%% @doc
%% @end
%%======================================================================
-module(leo_logger_appender_zmq).

-author('Yosuke Hara').
-vsn('0.9.0').

-behaviour(leo_logger_behavior).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, append/2, rotate/2]).

-define(DEF_ZMQ_BIND_PORT, 10501).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize this logger
%%
-spec(init(atom(), list(), list()) ->
             {ok, #logger_state{}}).
init(Appender, Callback, Props) ->
    {ok, #logger_state{appender_type = Appender,
                       appender_mod  = ?appender_mod(Appender),
                       props         = Props,
                       callback      = Callback}}.


%% @doc Append a message to the zmq
%%
-spec(append(binary(), #logger_state{}) ->
             ok).
append(Json, _State) ->
    Port = case application:get_env(leo_logger, zmq_bind_port) of
               undefined   -> ?DEF_ZMQ_BIND_PORT;
               {ok, Value} -> Value
           end,

    {ok, Context} = erlzmq:context(),
    {ok, Sender} = erlzmq:socket(Context, push),
    ok = erlzmq:bind(Sender, "tcp://*:" ++ integer_to_list(Port)),
    ok = erlzmq:send(Sender, Json),
    ok = erlzmq:close(Sender),
    ok = erlzmq:term(Context),
    ok.


%% @doc
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(_Hours, State) ->
    {ok, State}.

