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

-export([init/3, append/2, sync/1, format/2, rotate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize this logger
%%
-spec(init(atom(), list(), list()) ->
             {ok, #logger_state{}}).
init(Appender, Callback, Props) ->
    RootPath = leo_misc:get_value(?FILE_PROP_ROOT_PATH, Props),
    FileName = leo_misc:get_value(?FILE_PROP_FILE_NAME, Props),
    Level    = leo_misc:get_value(?FILE_PROP_LOG_LEVEL, Props),

    {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(now()),
    DateHour =  {Y, M, D, H},

    BasePath = filename:join(RootPath, FileName),
    {ok, Curr} = file:get_cwd(),
    BasePath1 = case BasePath of
                    "/"   ++ _Rest -> BasePath;
                    "../" ++ _Rest -> BasePath;
                    "./"  ++  Rest -> Curr ++ "/" ++ Rest;
                    _              -> Curr ++ "/" ++ BasePath
            end,
    BasePathLen = string:len(BasePath1),
    BasePath2   = case (BasePathLen == string:rstr(BasePath1, "/")) of
                      true  -> string:substr(BasePath1, 1, BasePathLen-1);
                      false -> BasePath1
                  end,

    case catch open(BasePath2, DateHour) of
        {'EXIT', Cause} ->
            {error, Cause};
        {CurrentFileName, Handler} ->
            {ok, #logger_state{appender_type = Appender,
                               appender_mod  = ?appender_mod(Appender),
                               props    = [{?FILE_PROP_FILE_NAME, BasePath2},
                                           {?FILE_PROP_CUR_NAME,  CurrentFileName},
                                           {?FILE_PROP_HANDLER,   Handler}],
                               callback  = Callback,
                               level     = Level,
                               hourstamp = DateHour}}
    end.


%% @doc Append a message to a file
%%
-spec(append(list(), #logger_state{}) ->
             ok).
append(#message_log{formatted_msg = FormattedMsg}, State) ->
    Handler = leo_misc:get_value(?FILE_PROP_HANDLER, State#logger_state.props),
    catch file:write(Handler, lists:flatten(FormattedMsg)),
    ok.


%% @doc Sync a file
%%
-spec(sync(#logger_state{}) ->
             ok | {error, any()}).
sync(State) ->
    Handler = leo_misc:get_value(?FILE_PROP_HANDLER, State#logger_state.props),
    file:datasync(Handler).


%% @doc Format a log message
%%
-spec(format(atom(), #message_log{}) ->
             list()).
format(_Appender, #message_log{format  = Format,
                               message = Message}) ->
    case catch io_lib:format(Format, Message) of
        {'EXIT', _Cause} ->
            [];
        Ret ->
            Ret
    end.


%% @doc Rotate a log file
%%
-spec(rotate(integer(), #logger_state{}) ->
             {ok, #logger_state{}}).
rotate(Hours, #logger_state{props = Props} = State) ->
    BaseFileName    = leo_misc:get_value(?FILE_PROP_FILE_NAME, Props),
    CurrentFileName = leo_misc:get_value(?FILE_PROP_CUR_NAME,  Props),
    Handler         = leo_misc:get_value(?FILE_PROP_HANDLER,   Props),

    ok = close(CurrentFileName, Handler),
    {NewLogFileName, NewHandler} = open(BaseFileName, Hours),
    {ok, State#logger_state{props    = [{?FILE_PROP_FILE_NAME, BaseFileName},
                                        {?FILE_PROP_CUR_NAME,  NewLogFileName},
                                        {?FILE_PROP_HANDLER,   NewHandler}],
                            hourstamp = Hours}}.

%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Open a log file
%% @private
open(BaseFileName, DateHour) ->
    _ = filelib:ensure_dir(BaseFileName),
    LogFileName = BaseFileName ++ suffix(DateHour),
    io:format("* opening log file is [~p]~n", [LogFileName]),

    {ok, Handler} = file:open(LogFileName, [read, write, raw]),
    {ok, Location} = file:position(Handler, eof),
    fix_log(Handler, Location),

    case file:read_link(BaseFileName) of
        {ok, _} ->
            ok = file:delete(BaseFileName);
        _ ->
            void
    end,
    _ = file:make_symlink(LogFileName, BaseFileName),
    {LogFileName, Handler}.


%% @doc Close a log file
%% @private
close(FileName, Handler) ->
    io:format("* closing log file: ~p~n", [FileName]),
    catch file:datasync(Handler),
    catch file:close(Handler),
    ok.


%% @doc Seek backwards to the last valid log entry
%% @private
fix_log(_Handler, 0) ->
    ok;
fix_log(Handler, 1) ->
    {ok, 0} = file:position(Handler, 0),
    ok;
fix_log(Handler, Location) ->
    case file:pread(Handler, Location - 1, 1) of
        {ok, [$\n | _]} ->
            ok;
        {ok, _} ->
            fix_log(Handler, Location - 1)
    end.


%% @doc Zero-padding number
%% @private
zeropad(Num, MinLength) ->
    NumStr = integer_to_list(Num),
    zeropad_str(NumStr, MinLength - length(NumStr)).
zeropad_str(NumStr, Zeros) when Zeros > 0 ->
    zeropad_str([$0 | NumStr], Zeros - 1);
zeropad_str(NumStr, _) ->
    NumStr.


%% @doc Create a suffix
%% @private
suffix({Y, M, D, H}) ->
    YS = zeropad(Y, 4),
    MS = zeropad(M, 2),
    DS = zeropad(D, 2),
    HS = zeropad(H, 2),
    lists:flatten([$., YS, MS, DS, $., HS]).

