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
%% Leo Logger - EUnit
%% @doc
%% @end
%%======================================================================
-module(leo_logger_util_tests).
-author('yosuke hara').

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

-define(TEST_LOG_ID,   'test').
-define(TEST_LOG_DIR,  "logs").
-define(TEST_LOG_FILE, "log").

-export([format/1]).

logger_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun append_/1,
                           fun message_1_/1
                          ]]}.

setup() ->
    ok.

teardown(_) ->
    os:cmd("rm -rf " ++ ?TEST_LOG_DIR),

    leo_logger_sup:stop(),
    application:stop(leo_logger),
    ok.

%%--------------------------------------------------------------------
%%% TEST FUNCTIONS
%%--------------------------------------------------------------------
append_(_) ->
    ok = leo_logger_util:new(?TEST_LOG_ID, ?LOG_APPENDER_FILE, [?MODULE, format],
                             ?TEST_LOG_DIR, ?TEST_LOG_FILE),

    Res = os:cmd("ls " ++ ?TEST_LOG_DIR),
    ?assertEqual(true, (Res /= [])),
    ok.

message_1_(_) ->
    ok = leo_logger_client_message:new(?TEST_LOG_DIR, 0),
    inspect(),
    ok.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
format(Log) ->
    Log.

inspect() ->
    ok = ?debug("test_log", "~p", [debug]),
    ok = ?info("test_log",  "~p", [info]),
    ok = ?warn("test_log",  "~p", [warn]),
    ok = ?error("test_log", "~p", [error]),
    ok = ?fatal("test_log", "~p", [fatal]),
    timer:sleep(1000),

    Error = {badarg,[{erlang,integer_to_list,[aaa],[]},
                     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
                     {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
                     {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,250}]},
                     {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
                     {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
                     {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]},
    ?error("test_log", "~p", [Error]),

    Res0 = os:cmd("ls " ++ ?TEST_LOG_DIR),
    Res1 = string:tokens(Res0, " \n"),
    ?assertEqual(true, (Res1 /= [])),

    Res2 = string:tokens(os:cmd("less " ++ ?TEST_LOG_DIR ++ "/" ++  lists:nth(1, Res1)), "\n"),
    Res3 = string:tokens(os:cmd("less " ++ ?TEST_LOG_DIR ++ "/" ++  lists:nth(2, Res1)), "\n"),

    ?assertEqual(true, (Res2 /= [])),
    ?assertEqual(true, (Res3 /= [])),
    ok.

-endif.

