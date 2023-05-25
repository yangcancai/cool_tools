%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 5月 2023 11:14
%%%-------------------------------------------------------------------
-module(cool_tools_simple_formatter).
-author("cam").
%% API
-export([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(Event, FConfig) ->
    try
        do_format(Event, FConfig)
    catch
        %% Errors during log formatting can lead to a death spiral of recursive error logging, so
        %% format the formatter error in a safe way and don't allow the exception to propagate.
        error:Reason:Stacktrace ->
            format_log_formatter_error(error, Reason, Stacktrace, Event, FConfig)
    end.

format_log_formatter_error(Class,
                           Reason,
                           Stacktrace,
                           #{meta := #{time := _Time}} = Event,
                           FConfig) ->
    H = header(Event#{level => error}),
    IoData =
        io_lib:format("~0p", [#{
                       what => log_format_failed,
                       class => Class,
                       reason => Reason,
                       stacktrace => Stacktrace,
                       formatter_module => ?MODULE,
                       original_event =>
                           unicode:characters_to_binary(
                               io_lib:format("~0p", [maps:remove(meta, Event)]))}]),
    IoData1 = all_to_binary_(unicode:characters_to_binary(IoData), FConfig),
    [H, IoData1, <<"\n">>].

-spec do_format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
do_format(E = #{msg :=
                    {report,
                     #{label := {error_logger, _},
                       format := Format,
                       args := Terms}}},
          FConfig) ->
    IoData = io_lib:format(Format, Terms),
    H = header(E),
    IoData1 = all_to_binary_(unicode:characters_to_binary(IoData), FConfig),
    [H, IoData1, <<"\n">>];
do_format(E = #{msg := {string, String}}, FConfig) ->
    IoData = io_lib:format(String, []),
    H = header(E),
    IoData1 = all_to_binary_(unicode:characters_to_binary(IoData), FConfig),
    [H, IoData1, <<"\n">>];
do_format(E = #{msg := {report, Report}}, FConfig) when is_map(Report) ->
    IoData = io_lib:format("~0p", [maps:remove(meta, Report)]),
    H = header(E),
    IoData1 = all_to_binary_(unicode:characters_to_binary(IoData), FConfig),
    [H, IoData1, <<"\n">>];
do_format(Map = #{msg := {Format, Terms}}, FConfig) ->
    IoData = io_lib:format(Format, Terms),
    H = header(Map),
    IoData1 = all_to_binary_(unicode:characters_to_binary(IoData), FConfig),
    [H, IoData1, <<"\n">>].
header(#{meta := #{time := Time, pid := Pid} = Meta, level := Level} = _Event) ->
  {M, F, _} = maps:get(mfa, Meta, {<<>>, <<>>, <<>>}),
   Line = maps:get(line, Meta, <<>>),
   case Line /= <<>> andalso M /= <<>>  of
     true ->
       io_lib:format("~ts [~p] ~p ~p:~p(~p) => ", [format_time(Time), Level, Pid, M, F, Line]);
     _->
       io_lib:format("~ts [~p] ~p => ", [format_time(Time), Level, Pid])
   end.
format_time(T) ->
    TimeString = calendar:system_time_to_rfc3339(T, [{unit, microsecond}]),
    unicode:characters_to_binary(TimeString).
all_to_binary_(Full, FConfig) ->
  all_to_binary(Full, maps:merge(default_config(), FConfig)).
all_to_binary(Full, FConfig) when is_binary(Full) ->
    Short = shorten_binary(Full, FConfig),
    ShortUnicode = unicode:characters_to_binary(Short, utf8, utf8),
    FullUnicode = unicode:characters_to_binary(Short, utf8, utf8),
    case {ShortUnicode, FullUnicode} of
        {<<_/binary>>, <<_/binary>>} ->
            ShortUnicode;
        {{incomplete, Incomplete, _}, <<_/binary>>} ->
            Incomplete;
        _ ->
        format_non_unicode(Full, FConfig)
    end.

format_non_unicode(Something, FConfig) ->
    Chars = format_str(Something, FConfig),
    unicode:characters_to_binary(Chars, utf8).

shorten_binary(S, #{format_chars_limit := unlimited}) ->
    S;
shorten_binary(S, #{format_chars_limit := L}) ->
    binary_part(S, 0, min(size(S), L)).

format_chars_limit_to_opts(unlimited) ->
    [];
format_chars_limit_to_opts(CharsLimit) ->
    [{chars_limit, CharsLimit}].

format_str(S, FConfig) ->
    do_format_str(S, FConfig).

do_format_str(S, #{format_depth := unlimited, format_chars_limit := L}) ->
    io_lib:format("~0tp", [S], format_chars_limit_to_opts(L));
do_format_str(S, #{format_depth := D, format_chars_limit := L}) ->
    io_lib:format("~0tP", [S, D], format_chars_limit_to_opts(L)).
default_config() ->
    #{format_chars_limit => unlimited}.
