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

-export([init/3, append/2, sync/1, format/2, rotate/2]).

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
                       props     = Props,
                       callback  = Callback,
                       level     = 0}}.


%% @doc Append a message to a file
%%
-spec(append(list(), #logger_state{}) ->
             ok).
append(#message_log{formatted_msg = FormattedMsg,
                    esearch = ESearch}, #logger_state{props = Props} = _State) ->
    Host    = leo_misc:get_value(?ESEARCH_PROP_HOST,    Props, ?DEF_ESEARCH_HOST),
    Port    = leo_misc:get_value(?ESEARCH_PROP_PORT,    Props, ?DEF_ESEARCH_PORT),
    Timeout = leo_misc:get_value(?ESEARCH_PROP_TIMEOUT, Props, ?DEF_ESEARCH_TIMEOUT),
    Index   = leo_misc:get_value(?ESEARCH_DOC_INDEX,    ESearch),
    Type    = leo_misc:get_value(?ESEARCH_DOC_TYPE,     ESearch),

    catch erlastic_search:index_doc(#erls_params{host     = list_to_binary(Host),
                                                 port     = Port,
                                                 timeout  = Timeout,
                                                 ctimeout = Timeout},
                                    Index, Type, FormattedMsg),
    ok.


%% @doc Sync a file
%%
-spec(sync(#logger_state{}) ->
             ok | {error, any()}).
sync(_State) ->
    ok.


%% @doc Format a log message
%%
-spec(format(atom(), #message_log{}) ->
             list()).
format(_Appender, #message_log{message = Message}) ->
    Message.


%% @doc Rotate a log file
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(Hours, #logger_state{props = Props} = State) ->
    Host    = leo_misc:get_value(?ESEARCH_PROP_HOST,    Props),
    Port    = leo_misc:get_value(?ESEARCH_PROP_PORT,    Props),
    Timeout = leo_misc:get_value(?ESEARCH_PROP_TIMEOUT, Props),
    catch erlastic_search:flush_all(#erls_params{host     = Host,
                                                 port     = Port,
                                                 timeout  = Timeout,
                                                 ctimeout = Timeout}),
    {ok, State#logger_state{hourstamp = Hours}}.

%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------

