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
-module(leo_logger_appender_file).

-author('Yosuke Hara').

-behaviour(leo_logger_behavior).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, append/2, rotate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize this logger
%%
-spec(init(atom(), list(), list()) ->
             {ok, #logger_state{}}).
init(Appender, Callback, Props) ->
    RootPath = proplists:get_value(?FILE_PROP_ROOT_PATH, Props),
    FileName = proplists:get_value(?FILE_PROP_FILE_NAME, Props),
    Level    = proplists:get_value(?FILE_PROP_LOG_LEVEL, Props),

    {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(now()),
    DateHour =  {Y, M, D, H},

    BaseFileName = filename:join(RootPath, FileName),
    filelib:ensure_dir(BaseFileName),

    case catch open(BaseFileName, DateHour) of
        {'EXIT', Cause} ->
            {error, Cause};
        {CurrentFileName, Handle} ->
            {ok, #logger_state{appender_type = Appender,
                               appender_mod  = ?appender_mod(Appender),
                               props    = [{?FILE_PROP_FILE_NAME, BaseFileName},
                                           {?FILE_PROP_CUR_NAME,  CurrentFileName},
                                           {?FILE_PROP_HANDLER,   Handle}],
                               callback  = Callback,
                               level     = Level,
                               hourstamp = DateHour}}
    end.


%% @doc Append a message to a file
%%
-spec(append(list(), #logger_state{}) ->
             ok).
append(FormattedMsg, State) ->
    Handle = proplists:get_value(?FILE_PROP_HANDLER, State#logger_state.props),
    catch file:write(Handle, lists:flatten(FormattedMsg)),
    ok.


%% @doc
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(Hours, #logger_state{props = Props} = State) ->
    BaseFileName    = proplists:get_value(?FILE_PROP_FILE_NAME, Props),
    CurrentFileName = proplists:get_value(?FILE_PROP_CUR_NAME,  Props),
    Handle          = proplists:get_value(?FILE_PROP_HANDLER,   Props),

    ok = close(CurrentFileName, Handle),
    {NewLogFileName, NewHandle} = open(BaseFileName, Hours),
    {ok, State#logger_state{props    = [{?FILE_PROP_FILE_NAME, BaseFileName},
                                        {?FILE_PROP_CUR_NAME,  NewLogFileName},
                                        {?FILE_PROP_HANDLER,   NewHandle}],
                            hourstamp = Hours}}.

%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
open(BaseFileName, DateHour) ->
    LogFileName = BaseFileName ++ suffix(DateHour),
    io:format("* opening log file is [~p]~n", [LogFileName]),

    {ok, FD} = file:open(LogFileName, [read, write, raw]),
    {ok, Location} = file:position(FD, eof),
    fix_log(FD, Location),
    file:truncate(FD),
    {LogFileName, FD}.


%% @private
close(FileName, FD) ->
    io:format("* closing log file: ~p~n", [FileName]),
    catch file:close(FD),
    ok.


%% @doc Seek backwards to the last valid log entry
%% @private
fix_log(_FD, 0) ->
    ok;
fix_log(FD, 1) ->
    {ok, 0} = file:position(FD, 0),
    ok;
fix_log(FD, Location) ->
    case file:pread(FD, Location - 1, 1) of
        {ok, [$\n | _]} ->
            ok;
        {ok, _} ->
            fix_log(FD, Location - 1)
    end.


%% @doc
%% @private
zeropad(Num, MinLength) ->
    NumStr = integer_to_list(Num),
    zeropad_str(NumStr, MinLength - length(NumStr)).
zeropad_str(NumStr, Zeros) when Zeros > 0 ->
    zeropad_str([$0 | NumStr], Zeros - 1);
zeropad_str(NumStr, _) ->
    NumStr.


%% @doc
%% @private
suffix({Y, M, D, H}) ->
    YS = zeropad(Y, 4),
    MS = zeropad(M, 2),
    DS = zeropad(D, 2),
    HS = zeropad(H, 2),
    lists:flatten([$., YS, MS, DS, $., HS]).

