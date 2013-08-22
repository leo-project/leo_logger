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
%% Leo Logger
%% @doc
%% @end
%%======================================================================
%% log-level
-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO,  1).
-define(LOG_LEVEL_WARN,  2).
-define(LOG_LEVEL_ERROR, 3).
-define(LOG_LEVEL_FATAL, 4).

%% log-appender (for message-log)
-define(LOG_APPENDER_FILE,   'file').
-define(LOG_APPENDER_AMQP,   'amqp').
-define(LOG_APPENDER_ZMQ,    'zmq').

-define(FILE_PROP_ROOT_PATH, 'root_path').
-define(FILE_PROP_FILE_NAME, 'org_filename').
-define(FILE_PROP_CUR_NAME,  'cur_filename').
-define(FILE_PROP_HANDLER,   'file_handler').
-define(FILE_PROP_LOG_LEVEL, 'log_level').

-define(LOG_ID_FILE_INFO,  'leo_logger_file_i').
-define(LOG_ID_FILE_ERROR, 'leo_logger_file_e').
-define(LOG_ID_ZMQ,        'leo_logger_zmq').
-define(LOG_ID_AMQP,       'leo_logger_amqp').


%%
-record(logger_state, {appender_type  :: atom(),
                       appender_mod   :: atom(),
                       callback       :: list(),
                       props          :: list(),
                       level = 0      :: integer(),
                       hourstamp = -1 :: integer()}).

-record(message_log,  {level         :: atom(),
                       module = []   :: string(),
                       function = [] :: string(),
                       line = 0      :: integer(),
                       format  = []  :: string(),
                       message = []  :: list()}).

-type(log_appender() :: ?LOG_APPENDER_FILE | ?LOG_APPENDER_AMQP | ?LOG_APPENDER_ZMQ).

%% macros.
%%
-define(appender_mod(AppenderType),
        case AppenderType of
            ?LOG_APPENDER_FILE -> leo_logger_appender_file;
            ?LOG_APPENDER_AMQP -> leo_logger_appender_amqp;
            ?LOG_APPENDER_ZMQ  -> leo_logger_appender_zmq;
            _ ->
                undefined
        end).


-define(log(Level, FuncName0, Format0, Message0),
        case Level of
            fatal   -> ?fatal(FuncName0, Format0, Message0);
            error   -> ?error(FuncName0, Format0, Message0);
            warning -> ?warn(FuncName0, Format0, Message0);
            info    -> ?info(FuncName0, Format0, Message0);
            debug   -> ?debug(FuncName0, Format0, Message0)
        end).

-define(log(Level, ModuleString1, FuncName1, Line1, Format1, Message1),
        %% Todo
        case Message1 of
            [] ->
                case Level of
                    fatal   -> ?fatal(ModuleString1, FuncName1, Line1, "~p", [Format1]);
                    error   -> ?error(ModuleString1, FuncName1, Line1, "~p", [Format1]);
                    warning -> ?warn(ModuleString1, FuncName1, Line1, "~p", [Format1]);
                    info    -> ?info(ModuleString1, FuncName1, Line1, "~p", [Format1]);
                    debug   -> ?debug(ModuleString1, FuncName1, Line1, "~p", [Format1])
                end;
            _ ->
                case Level of
                    fatal   -> ?fatal(ModuleString1, FuncName1, Line1, Format1, Message1);
                    error   -> ?error(ModuleString1, FuncName1, Line1, Format1, Message1);
                    warning -> ?warn(ModuleString1, FuncName1, Line1, Format1, Message1);
                    info    -> ?info(ModuleString1, FuncName1, Line1, Format1, Message1);
                    debug   -> ?debug(ModuleString1, FuncName1, Line1, Format1, Message1)
                end
        end).


-define(fatal(FuncName, Format, Message),
        leo_logger_client_message:fatal(#message_log{level    = ?LOG_LEVEL_FATAL,
                                                     module   = ?MODULE_STRING,
                                                     function = FuncName,
                                                     line     = ?LINE,
                                                     format   = Format,
                                                     message  = Message})).
-define(fatal(ModuleString, FuncName, Line, Format, Message),
        leo_logger_client_message:fatal(#message_log{level    = ?LOG_LEVEL_FATAL,
                                                     module   = ModuleString,
                                                     function = FuncName,
                                                     line     = Line,
                                                     format   = Format,
                                                     message  = Message})).

-define(error(FuncName, Format, Message),
        leo_logger_client_message:error(#message_log{level    = ?LOG_LEVEL_ERROR,
                                                     module   = ?MODULE_STRING,
                                                     function = FuncName,
                                                     line     = ?LINE,
                                                     format   = Format,
                                                     message  = Message})).
-define(error(ModuleString, FuncName, Line, Format, Message),
        leo_logger_client_message:error(#message_log{level    = ?LOG_LEVEL_ERROR,
                                                     module   = ModuleString,
                                                     function = FuncName,
                                                     line     = Line,
                                                     format   = Format,
                                                     message  = Message})).

-define(warn(FuncName, Format, Message),
        leo_logger_client_message:warn(#message_log{level    = ?LOG_LEVEL_WARN,
                                                    module   = ?MODULE_STRING,
                                                    function = FuncName,
                                                    line     = ?LINE,
                                                    format   = Format,
                                                    message  = Message})).
-define(warn(ModuleString, FuncName, Line, Format, Message),
        leo_logger_client_message:warn(#message_log{level    = ?LOG_LEVEL_WARN,
                                                    module   = ModuleString,
                                                    function = FuncName,
                                                    line     = Line,
                                                    format   = Format,
                                                    message  = Message})).

-define(info(FuncName, Format, Message),
        leo_logger_client_message:info(#message_log{level    = ?LOG_LEVEL_INFO,
                                                    module   = ?MODULE_STRING,
                                                    function = FuncName,
                                                    line     = ?LINE,
                                                    format   = Format,
                                                    message  = Message})).
-define(info(ModuleString, FuncName, Line, Format, Message),
        leo_logger_client_message:info(#message_log{level    = ?LOG_LEVEL_INFO,
                                                    module   = ModuleString,
                                                    function = FuncName,
                                                    line     = Line,
                                                    format   = Format,
                                                    message  = Message})).

-define(debug(FuncName, Format, Message),
        leo_logger_client_message:debug(#message_log{level    = ?LOG_LEVEL_DEBUG,
                                                     module   = ?MODULE_STRING,
                                                     function = FuncName,
                                                     line     = ?LINE,
                                                     format   = Format,
                                                     message  = Message})).
-define(debug(ModuleString, FuncName, Line, Format, Message),
        leo_logger_client_message:debug(#message_log{level    = ?LOG_LEVEL_DEBUG,
                                                     module   = ModuleString,
                                                     function = FuncName,
                                                     line     = Line,
                                                     format   = Format,
                                                     message  = Message})).

