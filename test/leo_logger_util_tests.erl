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
%% Leo Logger - EUnit
%% @doc
%% @end
%%======================================================================
-module(leo_logger_util_tests).
-author('Yosuke Hara').

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


suite_test_() ->
    {setup,
     fun () ->
             ok
     end,
     fun (_) ->
             application:stop(leo_logger),
             ok
     end,
     [
      {"test append messages in to the log-file#1",
       {timeout, 120, fun append/0}},
      {"test append messages in to the log-file#2",
       {timeout, 300, fun message/0}}
     ]}.


%%--------------------------------------------------------------------
%%% TEST FUNCTIONS
%%--------------------------------------------------------------------
append() ->
    ok = leo_logger_util:new(?TEST_LOG_ID, ?LOG_APPENDER_FILE, [?MODULE, format],
                             ?TEST_LOG_DIR, ?TEST_LOG_FILE),

    Res = os:cmd("ls " ++ ?TEST_LOG_DIR),
    ?assertEqual(true, (Res /= [])),
    ok.

message() ->
    ok = leo_logger_client_message:new(?TEST_LOG_DIR, 0),
    inspect(),
    append_messages(100),
    ok.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
format(Log) ->
    Log.

%% @private
inspect() ->
    ok = ?debug("test_log", "~p", [debug]),
    ok = ?debug("test_log", [{msg, debug}]),
    ok = ?debug("test_log", [{msg, debug},{cause, 'not_found'}]),

    ok = ?info("test_log",  "~p", [info]),
    ok = ?info("test_log", [{msg, info}]),
    ok = ?info("test_log", [{msg, info}, {cause, 'not_found'}]),

    ok = ?warn("test_log",  "~p", [warn]),
    ok = ?warn("test_log", [{msg, warn}]),
    ok = ?warn("test_log", [{msg, warn}, {cause, 'not_found'}]),

    ok = ?error("test_log", "~p", [error]),
    ok = ?error("test_log", [{msg, error}]),
    ok = ?error("test_log", [{msg, error}, {cause, 'not_found'}]),

    ok = ?fatal("test_log", "~p", [fatal]),
    ok = ?fatal("test_log", [{msg, fatal}]),
    ok = ?fatal("test_log", [{msg, fatal},{cause, 'not_found'}]),
    timer:sleep(timer:seconds(3)),

    Error = {badarg,[{erlang,integer_to_list,[aaa],[]},
                     {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,576}]},
                     {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,360}]},
                     {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,250}]},
                     {shell,exprs,7,[{file,"shell.erl"},{line,668}]},
                     {shell,eval_exprs,7,[{file,"shell.erl"},{line,623}]},
                     {shell,eval_loop,3,[{file,"shell.erl"},{line,608}]}]},
    ?error("test_log", "~p", [Error]),

    error_logger:error_msg("~p,~p,~p,~p~n",
                           [{module, ?MODULE_STRING}, {function, "inspect/0"},
                            {line, ?LINE},
                            {body, "TEST-1"}]),
    error_logger:warning_msg("~p,~p,~p,~p~n",
                             [{module, ?MODULE_STRING}, {function, "inspect/0"},
                              {line, ?LINE},
                              {body, "TEST-2"}]),
    error_logger:info_msg("~p,~p,~p,~p~n",
                          [{module, ?MODULE_STRING}, {function, "inspect/0"},
                           {line, ?LINE},
                           {body, "TEST-3"}]),

    Ret_1 = os:cmd("ls " ++ ?TEST_LOG_DIR),
    Ret_2 = string:tokens(Ret_1, " \n"),
    ?debugVal({length(Ret_2), Ret_2}),
    ?assertEqual(true, (Ret_2 /= [])),

    Ret_3 = string:tokens(os:cmd("less " ++ ?TEST_LOG_DIR ++ "/" ++  lists:nth(1, Ret_2)), "\n"),
    Ret_4 = string:tokens(os:cmd("less " ++ ?TEST_LOG_DIR ++ "/" ++  lists:nth(2, Ret_2)), "\n"),

    ?assertEqual(true, (Ret_3 /= [])),
    ?assertEqual(true, (Ret_4 /= [])),

    %% update the log-level
    ok = leo_logger_client_message:update_log_level(?LOG_LEVEL_WARN),
    ok = ?debug("test_log", "~p", [debug]),
    ok = ?debug("test_log", [{msg, debug}]),
    ok = ?debug("test_log", [{msg, debug},{cause, 'not_found'}]),
    ok = ?info("test_log",  "~p", [info]),
    ok = ?info("test_log", [{msg, info}]),
    ok = ?info("test_log", [{msg, info}, {cause, 'not_found'}]),
    Ret_5 = os:cmd("ls " ++ ?TEST_LOG_DIR),
    Ret_6 = string:tokens(Ret_5, " \n"),
    ?debugVal({length(Ret_6), Ret_6}),
    ?assertEqual(length(Ret_2), length(Ret_6)),
    ok.


%% @private
append_messages(0) ->
    ok;
append_messages(Index) ->
    Msg = lists:append(["test_log_", integer_to_list(Index)]),
    ok = ?debug(Msg, "~p", [debug]),
    ok = ?info(Msg,  "~p", [info]),
    ok = ?warn(Msg,  "~p", [warn]),
    ok = ?error(Msg, "~p", [error]),
    ok = ?fatal(Msg, "~p", [fatal]),
    timer:sleep(erlang:phash2(leo_date:now(), 100)),
    append_messages(Index - 1).

-endif.
