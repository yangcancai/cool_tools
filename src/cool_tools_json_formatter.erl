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
%%% Created : 2021-11-18T07:18:33+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_json_formatter).

-author("yangcancai").

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
                           #{meta := #{time := Time}} = Event,
                           FConfig) ->
    Meta = process_metadata(Event),
    IoData =
        jiffy:encode(#{'when' => format_time(Time),
                       level => error,
                       what => log_format_failed,
                       class => Class,
                       reason => Reason,
                       stacktrace => format_item(Stacktrace, FConfig),
                       formatter_module => ?MODULE,
                       original_event =>
                           unicode:characters_to_binary(
                               io_lib:format("~0p", [Event#{meta => Meta}]))}),
    [IoData, <<"\n">>].

-spec do_format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
do_format(E = #{msg :=
                    {report,
                     #{label := {error_logger, _},
                       format := Format,
                       args := Terms}}},
          FConfig) ->
    do_format(E#{msg :=
                     {report,
                      #{unstructured_log =>
                            unicode:characters_to_binary(
                                io_lib:format(Format, Terms))}}},
              FConfig);
do_format(E = #{msg := {string, String}}, FConfig) ->
    do_format(E#{msg :=
                     {report,
                      #{unstructured_log =>
                            unicode:characters_to_binary(
                                io_lib:format(String, []))}}},
              FConfig);
do_format(E = #{msg := {report, Report}}, FConfig) when is_map(Report) ->
    NewMap = process_metadata(E),
    NewReport = maps:merge(Report, NewMap),
    NewConfig = maps:merge(default_config(), config_correct_depth(FConfig)),
    Formatted = format_item(NewReport, NewConfig),
    IoData = jiffy:encode(Formatted),
    [IoData, <<"\n">>];
do_format(Map = #{msg := {Format, Terms}}, FConfig) ->
    do_format(Map#{msg :=
                       {report,
                        #{unstructured_log =>
                              unicode:characters_to_binary(
                                  io_lib:format(Format, Terms))}}},
              FConfig).

format_item(_Item, _FConfig = #{depth := 0}) ->
    <<"...">>;
format_item(Item, FConfig = #{depth := D}) when is_map(Item) ->
    ML = [{all_to_binary(Key, FConfig), format_item(Val, FConfig#{depth := D - 1})}
          || {Key, Val} <- maps:to_list(Item)],
    maps:from_list(ML);
format_item(Item, FConfig = #{depth := Depth}) when is_list(Item) ->
    case io_lib:printable_unicode_list(Item) of
        true ->
            % We want to print strings as strings
            format_str(Item, FConfig);
        false ->
            % And pass lists of objects as lists of objects
            [format_item(I, FConfig#{depth := Depth - 1}) || I <- Item]
    end;
format_item(Item, FConfig) ->
    all_to_binary(Item, FConfig).

format_time(T) ->
    TimeString = calendar:system_time_to_rfc3339(T, [{unit, microsecond}]),
    unicode:characters_to_binary(TimeString).

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
    end;
all_to_binary(Something, FConfig) ->
    format_non_unicode(Something, FConfig).

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

format_str(S, FConfig) when is_list(S) ->
    case io_lib:printable_unicode_list(S) of
        true ->
            B = unicode:characters_to_binary(S, utf8),
            shorten_binary(B, FConfig);
        false ->
            do_format_str(S, FConfig)
    end;
format_str(S, FConfig) when is_atom(S) ->
    format_str(atom_to_list(S), FConfig);
format_str(S, FConfig) ->
    do_format_str(S, FConfig).

do_format_str(S, #{format_depth := unlimited, format_chars_limit := L}) ->
    io_lib:format("~0tp", [S], format_chars_limit_to_opts(L));
do_format_str(S, #{format_depth := D, format_chars_limit := L}) ->
    io_lib:format("~0tP", [S, D], format_chars_limit_to_opts(L)).

process_metadata(#{meta :=
                       #{time := T,
                         mfa := {M, F, _},
                         pid := Pid,
                         line := Line},
                   level := L}) ->
    #{level => L,
      'when' => format_time(T),
      mfa =>
          erlang:list_to_binary(
              io_lib:format("~p:~p(~p)", [M, F, Line])),
      pid => Pid};
process_metadata(#{meta := #{time := T, pid := Pid}, level := L}) ->
    #{level => L,
      'when' => format_time(T),
      pid => Pid}.

config_correct_depth(C = #{depth := unlimited}) ->
    C#{depth := -1};
config_correct_depth(C) ->
    C.

default_config() ->
    #{format_chars_limit => unlimited,
      format_depth => unlimited,
      depth => -1}.
