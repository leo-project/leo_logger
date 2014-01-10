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
%% Leo Logger - API
%% @doc
%% @end
%%======================================================================
-module(leo_logger_util).

-author('Yosuke Hara').

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([new/3, new/4, new/5, new/6, add_appender/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc create a logger proc.
%%
-spec(new(atom(), log_appender(), list()) ->
             ok | {error, any()}).
new(Id, Appender, Callback) ->
    new(Id, Appender, Callback, []).

new(Id, Appender, Callback, Props) ->
    ok = start_app(),

    case supervisor:start_child(leo_logger_sup, [Id, Appender, Callback, Props]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        Error ->
            Error
    end.


-spec(new(atom(), log_appender(), list(), string(), string()) ->
             ok | {error, any()}).
new(Id, Appender, Callback, RootPath, FileName) ->
    new(Id, Appender, Callback, RootPath, FileName, 0).

-spec(new(atom(), log_appender(), list(), string(), string(), integer()) ->
             ok | {error, any()}).
new(Id, Appender, Callback, RootPath, FileName, Level) ->
    io:format("id:~p, path:~p, filename:~p~n", [Id, RootPath, FileName]),

    ok = start_app(),
    NewRootPath =
        case (string:len(RootPath) == string:rstr(RootPath, "/")) of
            true  -> RootPath;
            false -> RootPath ++ "/"
        end,

    case supervisor:start_child(leo_logger_sup,
                                [Id, Appender, Callback, [{?FILE_PROP_ROOT_PATH, NewRootPath},
                                                          {?FILE_PROP_FILE_NAME, FileName},
                                                          {?FILE_PROP_LOG_LEVEL, Level}]]) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        Error ->
            Error
    end.


%% @doc add an appender into the ets
%%
-spec(add_appender(atom(), atom()) ->
             ok).
add_appender(GroupId, LoggerId) ->
    catch ets:insert(?ETS_LOGGER_GROUP, {GroupId, LoggerId}),
    ok.


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc start object storage application.
%%
-spec(start_app() ->
             ok | {error, any()}).
start_app() ->
    Module = leo_logger,
    case application:start(Module) of
        ok ->
            ?ETS_LOGGER_GROUP = ets:new(?ETS_LOGGER_GROUP,
                                        [named_table, bag, public, {read_concurrency,true}]),
            ok;
        {error, {already_started, Module}} ->
            ok;
        Error ->
            Error
    end.

