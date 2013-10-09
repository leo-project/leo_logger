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
%% Leo Logger - Server
%% @doc
%% @end
%%======================================================================
-module(leo_logger_server).

-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/4, stop/1]).
-export([append/3, append/4, sync/1, rotate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Id, Appender, Callback, Props) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, Appender, Callback, Props], []).

stop(Id) ->
    gen_server:call(Id, stop, 30000).


%% @doc Append a message to a log-file.
%%
-spec(append(?LOG_APPEND_SYNC|?LOG_APPEND_ASYNC, atom(), any()) ->
             ok).
append(Method, Id, Log) ->
    append(Method, Id, Log, 0).
append(?LOG_APPEND_SYNC, Id, Log, Level) ->
    gen_server:call(Id, {append, {Id, Log, Level}});
append(?LOG_APPEND_ASYNC, Id, Log, Level) ->
    gen_server:cast(Id, {append, {Id, Log, Level}}).


%% @doc Sync a message to a log-file.
%%
-spec(sync(atom()) ->
             ok).
sync(Id) ->
    gen_server:call(Id, sync).


%% @doc Rotate a log-file.
%%
-spec(rotate(atom()) ->
             ok).
rotate(Id) ->
    gen_server:cast(Id, {rotate, Id}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Id, Appender, Callback, Props]) ->
    Mod = ?appender_mod(Appender),

    case catch erlang:apply(Mod, init, [Appender, Callback, Props]) of
        {ok, State} ->
            defer_rotate(Id),
            {ok, State};
        {'EXIT', Cause} ->
            {stop, Cause};
        {error, Cause} ->
            {stop, Cause}
    end.

handle_call({stop, Id}, _From, State) ->
    case Id of
        ?LOG_APPENDER_FILE ->
            case leo_misc:get_value(?FILE_PROP_HANDLER, State#logger_state.props) of
                undefined ->
                    void;
                Handle ->
                    file:close(Handle)
            end;
        _ ->
            void
    end,
    {stop, normal, ok, State};

handle_call({append, {Id, Log, Level}}, _From, #logger_state{level = RegisteredLevel,
                                                             buf_interval = 0} = State) ->
    NewState = case (Level >= RegisteredLevel) of
                   true  -> append_sub(Id, Log, State);
                   false -> State
               end,
    {reply, ok, NewState};

handle_call({append, {Id, Log, Level}}, _From, #logger_state{appender_type = Appender,
                                                             callback      = M,
                                                             level  = RegisteredLevel,
                                                             buffer = Buf,
                                                             buf_interval = BufInterval,
                                                             buf_begining = BufBegining
                                                            } = State) ->
    Now = leo_math:floor(leo_date:clock() / 1000),
    NewState = case ((Now - BufBegining) =< BufInterval) of
                   true ->
                       ?debugVal({Now, BufBegining, Now-BufBegining}),
                       case (Level >= RegisteredLevel) of
                           true ->
                               %% @TODO
                               {ok, FormattedLog} = format_log(M, Appender, Log),
                               ?debugVal(Log),
                               bulk_output([Log#message_log{formatted_msg = FormattedLog}|Buf], State);
                           false ->
                               State
                       end;
                   false ->
                       timer:apply_after(BufInterval, gen_server, call, [Id, bulk_output]),
                       {ok, FormattedLog} = format_log(M, Appender, Log),
                       ?debugVal(FormattedLog),
                       State#logger_state{
                         buffer = [Log#message_log{formatted_msg = FormattedLog}|Buf]}
               end,
    {reply, ok, NewState#logger_state{buf_begining = Now}};

handle_call(bulk_output, _From, #logger_state{appender_mod  = Mod,
                                              buffer = Buf} = State) ->
    ?debugVal({Mod, Buf}),
    NewState = bulk_output(Buf, State),
    {reply, ok, NewState};

handle_call(sync, _From, #logger_state{appender_mod = Mod} = State) ->
    catch erlang:apply(Mod, sync, [State]),
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({append, {Id, Log, Level}}, #logger_state{level = RegisteredLevel} = State) ->
    NewState = case (Level >= RegisteredLevel) of
                   true  -> append_sub(Id, Log, State);
                   false -> State
               end,
    {noreply, NewState};

handle_cast({rotate, Id}, State) ->
    {noreply, maybe_rotate(Id, State)}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc
%% @private
format_log(M, Appender, Log) ->
    case catch erlang:apply(M, format, [Appender, Log]) of
        {'EXIT', Cause0} ->
            Cause1 = element(1, Cause0),
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING}, {function, "append_sub/3"},
                                    {line, ?LINE}, {body, Cause1}]),
            {error, Cause1};
        FormattedLog when is_binary(FormattedLog) ->
            {ok, FormattedLog};
        FormattedLog when is_list(FormattedLog) ->
            {ok, FormattedLog};
        _ ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING}, {function, "append_sub/3"},
                                    {line, ?LINE}, {body, "Invalid formatted log message"}]),
            {error, invalid_format}
    end.

%% @doc
%% @private
append_sub(Id, Log, #logger_state{appender_mod  = Mod,
                                  appender_type = Appender,
                                  callback      = M} = State) ->
    case format_log(M, Appender, Log) of
        {ok, FormattedLog} when is_binary(FormattedLog) ->
            NewState = maybe_rotate(Id, State),
            append_sub_1(Mod,
                         Log#message_log{formatted_msg = FormattedLog}, NewState);
        {ok, FormattedLog} when is_list(FormattedLog) ->
            NewState = maybe_rotate(Id, State),
            append_sub_1(Mod,
                         Log#message_log{formatted_msg = lists:flatten(FormattedLog)}, NewState);
        {error,_Cause} ->
            State
    end.


%% @doc Append a message to LOG.
%% @private
-spec(append_sub_1(atom(), string()|binary(), #logger_state{}) ->
             #logger_state{}).
append_sub_1(undefined, _, State) ->
    State;
append_sub_1(Mod, LogMsg, State) ->
    erlang:apply(Mod, append, [LogMsg, State]).


%% @doc Output logs
%% @private
-spec(bulk_output(list(any()), #logger_state{}) ->
             ok | {error, any()}).
bulk_output(Logs, #logger_state{appender_mod = Mod} = State) ->
    %% @TODO
    ?debugVal({Mod, Logs, State}),
    erlang:apply(Mod, bulk_output, [Logs, State]).


%% @doc Defer for a rotating log
%% @private
-spec(defer_rotate(atom()) ->
             ok).
defer_rotate(Id) ->
    {_, {_, M, S}} = calendar:universal_time(),
    Time = 1000 * (3600 - ((M * 60) + S)),
    timer:apply_after(Time, ?MODULE, rotate, [Id]).


%% @doc Maybe a rotating log
%% @private
-spec(maybe_rotate(atom(), #logger_state{}) ->
             #logger_state{}).
maybe_rotate(Id, #logger_state{appender_mod = Module,
                               hourstamp    = HourStamp} = State) ->
    {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(now()),
    ThisHour = {Y, M, D, H},

    case (ThisHour == HourStamp) of
        true ->
            State;
        false ->
            defer_rotate(Id),

            case catch erlang:apply(Module, rotate, [ThisHour, State]) of
                {ok, NewState} ->
                    NewState;
                _ ->
                    State
            end
    end.

