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
%% @doc The file appender
%% @reference https://github.com/leo-project/leo_logger/blob/master/src/leo_logger_appender_file.erl
%% @end
%%======================================================================
-module(leo_logger_appender_file).

-author('Yosuke Hara').

-behaviour(leo_logger_behavior).

-include("leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/3, append/2, bulk_output/2,
         sync/1, format/2, rotate/2,
         close/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize the logger
%%
-spec(init(Appender, CallbackMod, Props) ->
             {ok, #logger_state{}} | {error, _} when Appender::atom(),
                                                     CallbackMod::module(),
                                                     Props::[{atom(), any()}]).
init(Appender, CallbackMod, Props) ->
    RootPath = leo_misc:get_value(?FILE_PROP_ROOT_PATH, Props),
    FileName = leo_misc:get_value(?FILE_PROP_FILE_NAME, Props),
    Level    = leo_misc:get_value(?FILE_PROP_LOG_LEVEL, Props),

    {{Y, M, D}, {H, _, _}} = calendar:now_to_local_time(os:timestamp()),
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
                               callback_mod  = CallbackMod,
                               props    = [{?FILE_PROP_FILE_NAME, BasePath2},
                                           {?FILE_PROP_CUR_NAME,  CurrentFileName},
                                           {?FILE_PROP_HANDLER,   Handler}],
                               level     = Level,
                               hourstamp = DateHour}}
    end.


%% @doc Append a message to a file
%%
-spec(append(Msg, State) ->
             #logger_state{} when Msg::#message_log{},
                                  State::#logger_state{}).
append(#message_log{formatted_msg = FormattedMsg}, State) ->
    Handler = leo_misc:get_value(?FILE_PROP_HANDLER, State#logger_state.props),
    catch file:write(Handler, lists:flatten(FormattedMsg)),
    State.


%% @doc Output messages
%%
-spec(bulk_output(Logs, State) ->
             #logger_state{} when Logs::[_],
                                  State::#logger_state{}).
bulk_output(_Logs, State) ->
    State.


%% @doc Format a log message
%%
-spec(format(Type, Msg) ->
             Ret when Type::split|bulk,
                      Msg::#message_log{},
                      Ret::string()).
format(_Type, #message_log{format  = Format,
                           message = Message}) ->
    case catch io_lib:format(Format, Message) of
        {'EXIT', _Cause} ->
            [];
        Ret ->
            Ret
    end.


%% @doc Synchronize the file
%%
-spec(sync(State) ->
             ok | {error, _} when State::#logger_state{}).
sync(State) ->
    Handler = leo_misc:get_value(?FILE_PROP_HANDLER, State#logger_state.props),
    file:datasync(Handler).


%% @doc Rotate the log file
%%
-spec(rotate(Hours, State) ->
             {ok, #logger_state{}} when Hours::{integer(),
                                                integer(),
                                                integer(),
                                                integer()},
                                        State::#logger_state{}).
rotate(Hours, #logger_state{props = Props} = State) ->
    BaseFileName    = leo_misc:get_value(?FILE_PROP_FILE_NAME, Props),
    CurrentFileName = leo_misc:get_value(?FILE_PROP_CUR_NAME,  Props),
    Handler         = leo_misc:get_value(?FILE_PROP_HANDLER,   Props),

    ok = close(BaseFileName, CurrentFileName, Handler),
    {NewLogFileName, NewHandler} = open(BaseFileName, Hours),
    {ok, State#logger_state{props = [{?FILE_PROP_FILE_NAME, BaseFileName},
                                     {?FILE_PROP_CUR_NAME,  NewLogFileName},
                                     {?FILE_PROP_HANDLER,   NewHandler}],
                            hourstamp = Hours}}.


%% @doc Close the log file
-spec(close(State) ->
             ok | {error, any()} when State::#logger_state{}).
close(#logger_state{props = Props} = _State) ->
    LinkedFileName  = leo_misc:get_value(?FILE_PROP_FILE_NAME, Props, []),
    CurrentFileName = leo_misc:get_value(?FILE_PROP_CUR_NAME,  Props, []),
    Handler         = leo_misc:get_value(?FILE_PROP_HANDLER,   Props),
    ok = close(LinkedFileName, CurrentFileName, Handler),
    ok.

%% @doc Close a log file
%% @private
close(LinkedFileName, FileName, Handler) ->
    io:format("* closing log file is ~s~n", [FileName]),
    catch file:datasync(Handler),
    catch file:close(Handler),
    case filelib:file_size(FileName) of
        0 ->
            %% if the files size is zero, it is removed
            catch file:delete(LinkedFileName),
            catch file:delete(FileName);
        _ ->
            void
    end,
    ok.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Open a log file
%% @private
open(BaseFileName, DateHour) ->
    _ = filelib:ensure_dir(BaseFileName),
    FileName = filename(BaseFileName, DateHour, 1),
    io:format("* opening log file is ~s~n", [FileName]),

    {ok, Handler}  = file:open(FileName, [read, write, raw]),
    {ok, Location} = file:position(Handler, eof),
    fix_log(Handler, Location),

    case file:read_link(BaseFileName) of
        {ok, _} ->
            ok = file:delete(BaseFileName);
        _ ->
            void
    end,
    file:make_symlink(FileName, BaseFileName),
    {FileName, Handler}.

%% @doc Create name of a new file
%% @private
filename(BaseFileName, DateHour, Branch) ->
    FileName = lists:append([BaseFileName,
                             suffix(DateHour),
                             ".", integer_to_list(Branch)
                            ]),
    case filelib:is_file(FileName) of
        true ->
            filename(BaseFileName, DateHour, Branch + 1);
        _ ->
            FileName
    end.


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
