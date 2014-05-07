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
%% ---------------------------------------------------------------------
%% Leo Logger - Application
%% @doc
%% @end
%%======================================================================
-module(leo_logger_app).

-author('Yosuke Hara').

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").

%% Application and Supervisor callbacks
-export([start/2, prep_stop/1, stop/1,
         profile_output/0]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    consider_profiling(),
    leo_logger_sup:start_link().

prep_stop(_State) ->
    leo_logger_sup:stop(),
    ok.

stop(_State) ->
    ok.


-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("leo_logger.procs.profile"),
    eprof:analyze(procs),
    eprof:log("leo_logger.total.profile"),
    eprof:analyze(total).


-spec consider_profiling() -> profiling | not_profiling | {error, any()}.
consider_profiling() ->
    case application:get_env(profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.
