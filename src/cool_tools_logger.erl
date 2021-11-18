%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
   
%%% @doc
%%%
%%% @end
%%% Created : 2021-11-18T04:19:36+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_logger).

-author("yangcancai").
-include("cool_tools_logger.hrl").
-export([set_global_loglevel/1]).
-export([get_global_loglevel/0]).
-export([set_module_loglevel/2]).
-export([clear_module_loglevel/1]).
-export([get_log_files/0]).
-export([dir/0]).
-export([loglevel_keyword_to_number/1]).
-export([start/0, stop/0, log/2, log/3, deprecated_logging/1, format_stacktrace_filter/2]).
%% Test API
-export([log_with_lvl/2]).

-define(DEPRECATION_TAB, cool_tools_deprecations).         % ETS table name
-define(DEFAULT_COOLDOWN_HOURS, 6).             % default cooldown time

-type deprecation_tag() :: any().              % Specifies the deprecation
-type log_level() :: warning | error.
-type unix_timestamp() :: mod_mam:unix_timestamp().
-type log_map() :: map().


-type atom_log_level() :: none | logger:level() | all.
-type int_log_level() :: -1..8.
-type level() :: atom_log_level() | int_log_level().

%% Sets primary log level
-spec get_global_loglevel() -> atom_log_level().
get_global_loglevel() ->
    maps:get(level, logger:get_primary_config()).

-spec set_global_loglevel(level()) ->
    ok | {error, {invalid_level, term()}}.
set_global_loglevel(Level) when is_integer(Level) ->
    set_global_loglevel(loglevel_number_to_keyword(Level));
set_global_loglevel(Level) ->
    logger:update_primary_config(#{level => Level}).

-spec set_module_loglevel(module(), level()) ->
    ok | {error, term()}.
set_module_loglevel(Module, Level) when is_integer(Level) ->
    set_module_loglevel(Module, loglevel_number_to_keyword(Level));
set_module_loglevel(Module, Level) ->
    logger:set_module_level(Module, Level).

-spec clear_module_loglevel(module()) -> ok | {error, term()}.
clear_module_loglevel(Module) ->
    set_module_loglevel(Module, get_global_loglevel()).

-spec get_log_files() -> [filename:name()].
get_log_files() ->
    [ File || #{config := #{file := File}} <- logger:get_handler_config() ].

-spec dir() -> string().
dir() ->
    case logger:get_handler_config(disk_log) of
        {ok, #{config := #{file := Path}}} ->
            filename:dirname(Path);
        _ ->
            ""
    end.

-spec loglevel_number_to_keyword(int_log_level())  -> atom_log_level().
loglevel_number_to_keyword(-1) -> none;
loglevel_number_to_keyword(0) -> emergency;
loglevel_number_to_keyword(1) -> alert;
loglevel_number_to_keyword(2) -> critical;
loglevel_number_to_keyword(3) -> error;
loglevel_number_to_keyword(4) -> warning;
loglevel_number_to_keyword(5) -> notice;
loglevel_number_to_keyword(6) -> info;
loglevel_number_to_keyword(7) -> debug;
loglevel_number_to_keyword(8) -> all.

-spec loglevel_keyword_to_number(atom_log_level()) -> int_log_level().
loglevel_keyword_to_number(none)      -> -1;
loglevel_keyword_to_number(emergency) -> 0;
loglevel_keyword_to_number(alert)     -> 1;
loglevel_keyword_to_number(critical)  -> 2;
loglevel_keyword_to_number(error)     -> 3;
loglevel_keyword_to_number(warning)   -> 4;
loglevel_keyword_to_number(notice)    -> 5;
loglevel_keyword_to_number(info)      -> 6;
loglevel_keyword_to_number(debug)     -> 7;
loglevel_keyword_to_number(all)       -> 8.


%% needed up
-spec start() -> ok.
start() ->
    prepare_ets(),
    set_global_loglevel(warning),
    ok.

%% @doc Used after using the module, when we won't log deprecation
%% messages again.
-spec stop() -> ok.
stop() ->
    destroy_ets(),
    ok.
deprecated_logging(Location) ->
    Map = #{what => deprecated_logging_macro,
            text => <<"Deprecated logging macro is used in your code">>},
    log(Location, Map, [{log_level, warning}]).

%% @doc Should be used to log deprecation messages. It logs
%% keeping proper frequency. Opts can be:
%%      * cooldown - the minimal interval (in milliseconds)
%%                   to be held between logs. Default: 6 hours
%%                   It is internally represented in microseconds
%%                   but API requires milliseconds.
%%      * log_level - 'warning' or 'error'
-spec log(deprecation_tag(), log_map(), proplists:proplist()) -> ok.
log(Tag, Msg, Opts) ->
    Ms = proplists:get_value(cooldown, Opts, default_cooldown()),
    Cooldown = milliseconds_to_microseconds(Ms),
    LogLvl = proplists:get_value(log_level, Opts, default_log_lvl()),
    maybe_log(Tag, Msg, LogLvl, Cooldown).

-spec log(deprecation_tag(), log_map()) -> ok.
log(Tag, Msg) ->
    log(Tag, Msg, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Deprecation table will hold pairs in form:
%%      {deprecation_tag(), unix_timestamp()}
%% and will indicate at what (unix) time last deprecation
%% warining was logged concerning a deprecation connected to
%% the deprecation tag specified in a key
-spec prepare_ets() -> ok.
prepare_ets() ->
    ets:new(?DEPRECATION_TAB, [{read_concurrency, true}, named_table, public]),
    ok.

-spec destroy_ets() -> ok.
destroy_ets() ->
    ets:delete(?DEPRECATION_TAB),
    ok.

-spec maybe_log(deprecation_tag(), log_map(), log_level(), unix_timestamp()) -> ok.
maybe_log(Tag, Msg, Lvl, Cooldown) ->
    Timestamp = case ets:lookup(?DEPRECATION_TAB, Tag) of
                             [] ->
                                not_logged;
                             [{Tag, LastLogged}] ->
                                 LastLogged
                         end,
    case did_cooldown_elapse(Timestamp, Cooldown) of
        true ->
            ?MODULE:log_with_lvl(Msg, Lvl),     % ?MODULE lets meck mock it
            ets:insert(?DEPRECATION_TAB, {Tag, os:timestamp()}),
            ok;
        false ->
            ok
    end.

-spec did_cooldown_elapse(unix_timestamp() | 'not_logged', unix_timestamp())
        -> boolean().
did_cooldown_elapse(not_logged, _) -> true;
did_cooldown_elapse(LastLogged, Cooldown) ->
    Now = os:timestamp(),
    timer:now_diff(Now, LastLogged) > Cooldown.

-spec default_cooldown() -> unix_timestamp().
default_cooldown() -> ?DEFAULT_COOLDOWN_HOURS * 3600000000.

-spec default_log_lvl() -> log_level().
default_log_lvl() -> error.

-spec log_with_lvl(log_map(), log_level()) -> ok.
log_with_lvl(Msg, error) ->
    ?LOG_ERROR(Msg);
log_with_lvl(Msg, warning) ->
    ?LOG_WARNING(Msg).

-spec milliseconds_to_microseconds(Milliseconds :: integer()) 
        -> unix_timestamp().
milliseconds_to_microseconds(N) -> N * 1000.


format_stacktrace_filter(Event=#{msg := {report, Msg=#{stacktrace := S}}}, _) ->
    Event#{msg => {report, Msg#{stacktrace => format_stacktrace(S)} }};
format_stacktrace_filter(Event, _) ->
    Event.
format_stacktrace(Stacktrace) ->
    iolist_to_binary(do_format_stacktrace(Stacktrace)).

do_format_stacktrace([{Mod,Fun,Arity,Info}|T]) ->
    Line = proplists:get_value(line, Info, 0),
    H = io_lib:format("~p:~p/~p:~p", [Mod, Fun, Arity, Line]),
    more_format_stacktrace(H, T);
do_format_stacktrace([Other|T]) ->
    H = io_lib:format("~p", [Other]),
    more_format_stacktrace(H, T);
do_format_stacktrace([]) ->
    [].

more_format_stacktrace(H, []) ->
    [H];
more_format_stacktrace(H, T) ->
    [H, " "|do_format_stacktrace(T)].