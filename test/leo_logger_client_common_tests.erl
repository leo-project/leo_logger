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
-module(leo_logger_client_common_tests).
-author('yosuke hara').

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

-define(TEST_LOG_DIR,  "logs").
-define(TEST_LOG_FILE, "access").

logger_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun append_/1
                          ]]}.

setup() ->
    application:start(leo_logger),
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
    LogId = 'leo_logger_access',
    ok = leo_logger_client_common:new('log_group_access', LogId,
                                      ?TEST_LOG_DIR, "access"),

    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["GET", "leo/fast/storage/system/1", 13075,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["GET", "leo/fast/storage/system/2", 13076,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["GET", "leo/fast/storage/system/3", 13077,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["GET", "leo/fast/storage/system/4", 13078,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["GET", "leo/fast/storage/system/5", 13079,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["PUT", "leo/fast/storage/system/11", 13075,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["PUT", "leo/fast/storage/system/12", 13076,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["PUT", "leo/fast/storage/system/13", 13077,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["PUT", "leo/fast/storage/system/14", 13078,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    ok = leo_logger_client_common:append({LogId,
                                          #message_log{format  = "[~s]\t~s\t~w\t\~s\t~w\n",
                                                       message = ["PUT", "leo/fast/storage/system/15", 13079,
                                                                  leo_date:date_format(type_of_now, now()),
                                                                  unixtime()
                                                                 ]}}),
    inspect(LogId),
    ok.



%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
inspect(LogId) ->
    leo_logger_client_common:sync(LogId),

    Res0 = os:cmd("ls " ++ ?TEST_LOG_DIR),
    Res1 = string:tokens(Res0, " \n"),
    ?assertEqual(true, (Res1 /= [])),
    ok.

unixtime() ->
    {H,S,_} = os:timestamp(),
    1000000 * H + S.

-endif.

