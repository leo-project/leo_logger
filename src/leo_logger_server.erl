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
-export([append/3, append/4,
         bulk_output/1, sync/1, rotate/1,
         close/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Id, Appender, CallbackMod, Props) ->
    gen_server:start_link({local, Id}, ?MODULE, [Appender, CallbackMod, Props], []).

stop(Id) ->
    gen_server:call(Id, stop, 30000).


%% @doc Append a message to a log-file.
%%
-spec(append(?LOG_APPEND_SYNC|?LOG_APPEND_ASYNC, atom(), any()) ->
             ok).
append(Method, Id, Log) ->
    append(Method, Id, Log, 0).
append(?LOG_APPEND_SYNC, Id, Log, Level) ->
    gen_server:call(Id, {append, {Log, Level}});
append(?LOG_APPEND_ASYNC, Id, Log, Level) ->
    gen_server:cast(Id, {append, {Log, Level}}).


%% @doc output bulked message to a log-file.
%%
-spec(bulk_output(atom()) ->
             ok).
bulk_output(Id) ->
    gen_server:call(Id, bulk_output).


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
    gen_server:cast(Id, rotate).


%% @doc Close a logger
%%
-spec(close(atom()) ->
             ok).
close(Id) ->
    gen_server:call(Id, close).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Appender, CallbackMod, Props]) ->
    Mod = ?appender_mod(Appender),

    case catch erlang:apply(Mod, init, [Appender, CallbackMod, Props]) of
        {ok, State} ->
            {ok, State#logger_state{
                   buf_begining = leo_math:floor(leo_date:clock() / 1000)}};
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

handle_call({append, {Log, Level}}, _From, #logger_state{level = RegisteredLevel,
                                                         buf_duration = 0} = State) ->
    NewState = case (Level >= RegisteredLevel) of
                   true  -> append_sub(Log, State);
                   false -> State
               end,
    {reply, ok, NewState};

handle_call({append, {Id, Log, Level}}, _From, #logger_state{callback_mod = M,
                                                             level  = RegisteredLevel,
                                                             buffer = Buf,
                                                             buf_duration  = BufInterval,
                                                             buf_begining  = BufBegining,
                                                             is_buf_output = IsBufOutput
                                                            } = State) ->
    Now = leo_math:floor(leo_date:clock() / 1000),
    State_2 = case ((Now - BufBegining) < BufInterval) of
                  true ->
                      State_1 = case IsBufOutput of
                                    true ->
                                        State;
                                    false ->
                                        timer:apply_after(BufInterval, ?MODULE, bulk_output, [Id]),
                                        State#logger_state{is_buf_output = true}
                                end,
                      {ok, FormattedLog} = format_log(M, bulk, Log),
                      State_1#logger_state{
                        buffer = [Log#message_log{formatted_msg = FormattedLog}|Buf]};
                  false ->
                      case (Level >= RegisteredLevel) of
                          true ->
                              {ok, FormattedLog} = format_log(M, bulk, Log),
                              bulk_output_sub([Log#message_log{formatted_msg = FormattedLog}|Buf], State);
                          false ->
                              State
                      end
              end,
    {reply, ok, State_2#logger_state{buf_begining = Now}};


handle_call(bulk_output, _From, #logger_state{buffer = [] } = State) ->
    {reply, ok, State#logger_state{is_buf_output = false}};
handle_call(bulk_output, _From, #logger_state{buffer = Buf} = State) ->
    State_1 = bulk_output_sub(Buf, State),
    {reply, ok, State_1#logger_state{is_buf_output = false}};

handle_call(sync, _From, #logger_state{appender_mod = Mod} = State) ->
    catch erlang:apply(Mod, sync, [State]),
    {reply, ok, State};

handle_call(close, _From, #logger_state{appender_mod = Mod} = State) ->
    catch erlang:apply(Mod, close, [State]),
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({append, {Log, Level}}, #logger_state{level = RegisteredLevel} = State) ->
    NewState = case (Level >= RegisteredLevel) of
                   true  -> append_sub(Log, State);
                   false -> State
               end,
    {noreply, NewState};

handle_cast(rotate, State) ->
    {noreply, defer_rotate(State)}.


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
format_log(M, Type, Log) ->
    case catch erlang:apply(M, format, [Type, Log]) of
        {'EXIT', Cause0} ->
            Cause1 = element(1, Cause0),
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING}, {function, "append_sub/2"},
                                    {line, ?LINE}, {body, Cause1}]),
            {error, Cause1};
        FormattedLog ->
            {ok, FormattedLog}
    end.

%% @doc
%% @private
append_sub(Log, #logger_state{appender_mod = Mod,
                              callback_mod = M} = State) ->
    case format_log(M, split, Log) of
        {ok, FormattedLog} when is_binary(FormattedLog) ->
            append_sub_1(Mod,
                         Log#message_log{formatted_msg = FormattedLog}, State);
        {ok, FormattedLog} when is_list(FormattedLog) ->
            append_sub_1(Mod,
                         Log#message_log{formatted_msg = lists:flatten(FormattedLog)}, State);
        {error,_Cause} ->
            State
    end.


%% @doc Append a message to LOG.
%% @private
-spec(append_sub_1(atom(), #message_log{}, #logger_state{}) ->
             #logger_state{}).
append_sub_1(undefined, _, State) ->
    State;
append_sub_1(Mod, LogMsg, State) ->
    erlang:apply(Mod, append, [LogMsg, State]).


%% @doc Output logs
%% @private
-spec(bulk_output_sub(list(any()), #logger_state{}) ->
             #logger_state{}).
bulk_output_sub(Logs, #logger_state{appender_mod = Mod} = State) ->
    erlang:apply(Mod, bulk_output, [Logs, State]).


%% @doc Defer for a rotating log
%% @private
-spec(defer_rotate(#logger_state{}) ->
             ok).
defer_rotate(#logger_state{appender_mod = Module,
                           hourstamp    = HourStamp} = State) ->
    {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(now()),
    ThisHour = {Y, M, D, H},

    case (ThisHour == HourStamp) of
        true ->
            State;
        false ->
            case catch erlang:apply(Module, rotate, [ThisHour, State]) of
                {ok, NewState} ->
                    NewState;
                _ ->
                    State
            end
    end.
