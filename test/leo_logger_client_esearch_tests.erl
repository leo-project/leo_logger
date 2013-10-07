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
-module(leo_logger_client_esearch_tests).
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
    leo_logger_sup:stop(),
    application:stop(leo_logger),
    ok.

%%--------------------------------------------------------------------
%%% TEST FUNCTIONS
%%--------------------------------------------------------------------
append_(_) ->
    LogId = 'leo_logger_access',
    ok = leo_logger_client_esearch:new('log_group_access', LogId, "127.0.0.1", 9200, 5000),
    Timestamp = list_to_binary(leo_date:date_format('utc', calendar:datetime_to_gregorian_seconds(
                                                             calendar:now_to_universal_time(os:timestamp())))),
    Msg1 = [{<<"@timestamp">>, Timestamp},
            {<<"@bucket">>,    <<"bucket_1">>},
            {<<"@message">>, <<"211.170.104.56 - - [03/Oct/2013:08:54:55 +0000] \"GET /EHHK.html HTTP/1.1\" 503 0 \"-\" \"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322)\"">>},
            {<<"extension">>, <<"html">>},
            {<<"clientip">>, <<"211.170.104.56">>},
            {<<"request">>, <<"/bucket1/ADC.html">>},
            {<<"response">>, <<"200">>},
            {<<"bytes">>, <<"512">>}],
    Msg2 = [{<<"@timestamp">>, Timestamp},
            {<<"@bucket">>,    <<"bucket_1">>},
            {<<"@message">>, <<"211.170.104.56 - - [03/Oct/2013:08:54:55 +0000] \"PUT /EHHK.html HTTP/1.1\" 503 0 \"-\" \"Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322)\"">>},
            {<<"extension">>, <<"html">>},
            {<<"clientip">>, <<"211.170.104.56">>},
            {<<"request">>, <<"/bucket1/ABC.html">>},
            {<<"response">>, <<"200">>},
            {<<"bytes">>, <<"1024">>}],

    ok = leo_logger_client_esearch:append({LogId, #message_log{message = Msg1,
                                                               esearch = [{?ESEARCH_DOC_INDEX, <<"2013-10-03">>},
                                                                          {?ESEARCH_DOC_TYPE,  <<"bucket_1">>}]
                                                              }}),
    ok = leo_logger_client_esearch:append({LogId, #message_log{message = Msg2,
                                                               esearch = [{?ESEARCH_DOC_INDEX, <<"2013-10-03">>},
                                                                          {?ESEARCH_DOC_TYPE,  <<"bucket_1">>}]
                                                              }}),

    inspect(),
    ok.



%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
inspect() ->
    Ret = erlastic_search:search(<<"2013-10-03">>, <<"bucket_1">>, <<"extension:html">>),
    ?debugVal(Ret),
    ?assertMatch({ok, _Ret}, Ret),
    ok.

-endif.

