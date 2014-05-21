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
%% Leo Logger - Supervisor.
%% @doc
%% @end
%%======================================================================
-module(leo_logger_sup).

-author('Yosuke Hara').

-behaviour(supervisor).

-include_lib("eunit/include/eunit.hrl").

%% External API
-export([start_link/0,
         stop/0]).

%% Callbacks
-export([init/1]).

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @doc start link.
%% @end
-spec start_link() ->
      {'ok', pid()} | 'ignore' | {'error',_}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc stop process.
%% @end
-spec stop() -> 'ok' | 'not_started'.
stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) == true ->
            List = supervisor:which_children(Pid),
            ok = close_logger(List),
            ok;
        _ -> not_started
    end.

%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @doc stop process.
%% @end
%% @private
init([]) ->
    {ok, {{one_for_one, 5, 60}, []}}.

%% ---------------------------------------------------------------------
%% Inner Function(s)
%% ---------------------------------------------------------------------
%% @doc Stop a logger file
%% @private
close_logger([]) ->
    ok;
close_logger([{Id,_Pid, worker, ['leo_logger_server' = Mod|_]}|T]) ->
    ok = Mod:close(Id),
    close_logger(T);
close_logger([_|T]) ->
    close_logger(T).

