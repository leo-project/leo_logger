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
%% Leo Logger - File Appender
%% @doc
%% @end
%%======================================================================
-module(leo_logger_appender_esearch).

-author('Yosuke Hara').

-behaviour(leo_logger_behavior).

-include("leo_logger.hrl").
-include_lib("erlastic_search/include/erlastic_search.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, append/2, bulk_output/2, sync/1, format/2, rotate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize this logger
%%
-spec(init(atom(), list(), list()) ->
             {ok, #logger_state{}}).
init(Appender, CallbackMod, Props) ->
    {ok, #logger_state{appender_type = Appender,
                       appender_mod  = ?appender_mod(Appender),
                       props         = Props,
                       callback_mod  = CallbackMod,
                       level         = 0,
                       buf_interval  = ?DEF_ESEARCH_BUF_INTERVAL,
                       buf_begining  = 0
                      }}.


%% @doc Append a message to a file
%%
-spec(append(list(), #logger_state{}) ->
             #logger_state{}).
append(#message_log{formatted_msg = FormattedMsg,
                    esearch = ESearch}, #logger_state{props = Props} = State) ->
    {Host, Port, Timeout} = get_env(Props),
    Index = leo_misc:get_value(?ESEARCH_DOC_INDEX, ESearch),
    Type  = leo_misc:get_value(?ESEARCH_DOC_TYPE,  ESearch),

    catch erlastic_search:index_doc(#erls_params{host     = Host,
                                                 port     = Port,
                                                 timeout  = Timeout,
                                                 ctimeout = Timeout},
                                    Index, Type, FormattedMsg),
    State.


-spec(bulk_output(list(any()), #logger_state{}) ->
             #logger_state{}).
bulk_output(Logs, #logger_state{props = Props} = State) ->
    {Host, Port, Timeout} = get_env(Props),
    spawn(fun() ->
                  Fun  = fun(#message_log{formatted_msg = Msg}) ->
                                 {Index, Type, Id, Doc} = Msg,
                                 Header = jsx:encode(
                                            [{<<"index">>,
                                              [
                                               {<<"_index">>, Index},
                                               {<<"_type">>, Type},
                                               {<<"_id">>, Id}
                                              ]
                                             }
                                            ]),
                                 [Header, <<"\n">>, jsx:encode(Doc), <<"\n">>]
                         end,
                  Params = #erls_params{host     = Host,
                                        port     = Port,
                                        timeout  = Timeout,
                                        ctimeout = Timeout},
                  Body = lists:map(Fun, Logs),
                  catch erls_resource:post(
                          Params, <<"/_bulk">>, [], [], iolist_to_binary(Body), [])
          end),
    State#logger_state{buffer = []}.


%% @doc Sync a file
%%
-spec(sync(#logger_state{}) ->
             ok | {error, any()}).
sync(_State) ->
    ok.


%% @doc Format a log message
%%
-spec(format(split|bulk, #message_log{}) ->
             list()).
format(split, #message_log{message = Message}) ->
    Message;
format(bulk, #message_log{message = Message,
                          esearch = ESearch}) ->
    Index = leo_misc:get_value(?ESEARCH_DOC_INDEX, ESearch),
    Type  = leo_misc:get_value(?ESEARCH_DOC_TYPE,  ESearch),
    Id = << (list_to_binary(atom_to_list(node())))/binary,
            "-",
            (list_to_binary(integer_to_list(leo_date:clock())))/binary >>,
    {Index, Type, Id, Message}.


%% @doc Rotate a log file
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(Hours, #logger_state{props = Props} = State) ->
    {Host, Port, Timeout} = get_env(Props),
    catch erlastic_search:flush_all(#erls_params{host     = Host,
                                                 port     = Port,
                                                 timeout  = Timeout,
                                                 ctimeout = Timeout}),
    {ok, State#logger_state{hourstamp = Hours}}.

%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
get_env(Props) ->
    Host    = leo_misc:get_value(?ESEARCH_PROP_HOST,    Props, ?DEF_ESEARCH_HOST),
    Port    = leo_misc:get_value(?ESEARCH_PROP_PORT,    Props, ?DEF_ESEARCH_PORT),
    Timeout = leo_misc:get_value(?ESEARCH_PROP_TIMEOUT, Props, ?DEF_ESEARCH_TIMEOUT),
    {list_to_binary(Host), Port, Timeout}.

