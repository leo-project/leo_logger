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
%% @doc The log rotator
%% @reference https://github.com/leo-project/leo_logger/blob/master/src/leo_logger_rotator.erl
%% @end
%%======================================================================
-module(leo_logger_rotator).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,
         start_link/3,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-define(DEF_ROTATION_INTERVAL, timer:seconds(10)).
-else.
-define(DEF_ROTATION_INTERVAL, timer:minutes(1)).
-endif.

-record(state, {id  :: atom(),
                mod :: atom(),
                interval = timer:minutes(10) :: pos_integer()}).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(ServerId, Mod) ->
             {ok, pid()} | ignore | {error, any()} when ServerId::atom(),
                                                        Mod::module()).
start_link(ServerId, Mod) ->
    gen_server:start_link(?MODULE, [ServerId, Mod, ?DEF_ROTATION_INTERVAL], []).

-spec(start_link(ServerId, Mod, RotationInterval) ->
             {ok, pid()} | ignore | {error, any()} when ServerId::atom(),
                                                        Mod::atom(),
                                                        RotationInterval::pos_integer()).
start_link(ServerId, Mod, RotationInterval) ->
    gen_server:start_link(?MODULE, [ServerId, Mod, RotationInterval], []).


%% @doc Stop the server
-spec(stop(Pid) ->
             ok when Pid::pid()).
stop(Pid) ->
    gen_server:cast(Pid, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% @doc Initiates the server
init([ServerId, Mod, RotationInterval]) ->
    {ok, #state{id = ServerId,
                mod = Mod,
                interval = RotationInterval}, RotationInterval}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(_Request, _From, #state{interval = RotationInterval} = State) ->
    Reply = ok,
    {reply, Reply, State, RotationInterval}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, #state{interval = RotationInterval} = State) ->
    {noreply, State, RotationInterval}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(timeout, State=#state{id = ServerId,
                                  mod = Mod,
                                  interval = RotationInterval}) ->
    catch Mod:rotate(ServerId),
    {noreply, State, RotationInterval}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason, _State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
