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
%%% Created : 2021-09-08T09:21:17+00:00
%%%-------------------------------------------------------------------

-module(cool_tools).

-author("yangcancai").

%% API
%% data type transfer
%% ================================

-export([to_binary/1]).
-export([to_binary/2]).
-export([to_utf8_binary/1]).
-export([to_list/1]).
-export([to_list/2]).
-export([to_integer/1]).
-export([to_atom/1]).
-export([to_atom/2]).
%% time
%% =================================
-export([to_second/1]).
-export([to_msec/1]).
-export([to_msec/2]).
-export([timestamp_to_localtime/1]).
-export([localtime_to_timestamp/1]).
-export([timestamp/0]).
-export([epoch/0]).
-export([midnight/0]).
-export([get_date_long/0]).
-export([to_date_long/1]).
-export([get_date_short/0]).
-export([to_date_short/1]).
-export([get_time_long/0]).
-export([get_time_short/0]).
-export([to_time_long/1]).
-export([to_time_short/1]).
-export([diff_day/2]).
-export([diff_daytime/2]).
-export([diff_next_daytime/2]).
-export([diff_next_hour/0]).
-export([days/0]).
-export([days_to_date/1]).
-export([time_to_days/1]).
-export([time_to_hours/1]).
-export([get_pre_date_long/1]).
-export([get_pre_date_short/1]).
-export([hours/0]).
-export([hours/1]).
-export([format_hour/1]).
-export([format_minute/1]).
-export([format_second/1]).
-export([first_day_of_this_month/0]).
-export([first_day_of_this_month/1]).
-export([first_day_of_next_month/0]).
-export([first_day_of_next_month/1]).
-export([next_month/0]).
-export([next_month/1]).
%% decode and encode
%% ========================================
-export([md5/1]).
-export([sha1/1]).
-export([decode_json/1]).
-export([endcode_json/1]).
-export([rand_str/0]).
-export([encode_token/2]).
-export([decode_token/3]).
-export([safe_url_2_base64/1]).
-export([base64_2_safe_url/1]).
-export([encode/1]).
-export([decode/1]).
-export([encode_mime/1]).
-export([decode_pagekey/1]).
-export([encode_pagekey/2]).
-export([decode_url_pagekey/1]).
-export([encode_url_pagekey/2]).
-export([string_to_term/1]).
-export([term_to_string/1]).
%% for
%% ==========================================
-export([for/3]).
-export([if_/3]).
-export([pmap/2]).
%%% uuid
%%% %% ==========================================
-export([uuid_v1/0]).
-export([uuid_v1_string/0]).
-export([uuid_v1_string/1]).
-export([uuid_v1_int/0]).
-export([uuid_v4/0]).
-export([uuid_v4_string/0]).
-export([uuid_v4_string/1]).
-export([uuid_v4_int/0]).
-export([uuid_to_string/2]).
-export([uuid_to_string/1]).
%% other
%% ================================================
-export([list_join/2]).
-export([to_upper/1, encode_login_password/2]).
-export([count_mixed_chars/1]).
-export([count_chinese_chars/1]).
-export([os_cmd/1]).
-export([total_queue_len/0]).
-export([total_process_len/0]).

to_binary(V) when is_integer(V) ->
    erlang:integer_to_binary(V);
to_binary(V) when is_list(V) ->
    erlang:list_to_binary(V);
to_binary(V) when is_atom(V) ->
    erlang:atom_to_binary(V, utf8);
to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_float(V) ->
    to_binary(V, 4).

to_binary(V, N) when is_float(V), is_integer(N) ->
    erlang:float_to_binary(V, [{decimals, N}]).

-spec to_utf8_binary(Val) -> Result
    when Val :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata(),
         Result :: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()},
         RestData :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata().
to_utf8_binary(Val) ->
    case unicode:characters_to_binary(Val, utf8) of
        {error, _, _} ->
            unicode:characters_to_binary(Val, latin1);
        UnicodeValue ->
            UnicodeValue
    end.

to_list(V) when is_binary(V) ->
    erlang:binary_to_list(V);
to_list(V) when is_atom(V) ->
    erlang:atom_to_list(V);
to_list(V) when is_integer(V) ->
    erlang:integer_to_list(V);
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_float(V) ->
    to_list(V, 4).

to_list(V, N) when is_float(V) ->
    erlang:float_to_list(V, [{decimals, N}]).

to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:binary_to_integer(V);
to_integer(V) when is_integer(V) ->
    V.

to_atom(V) when is_atom(V) ->
    V;
to_atom(V) when is_integer(V) ->
    to_atom(erlang:integer_to_binary(V));
to_atom(V) when is_list(V) ->
    erlang:list_to_atom(V);
to_atom(V) when is_binary(V) ->
    erlang:binary_to_atom(V, utf8);
to_atom(V) when is_float(V) ->
    to_atom(V, 4).

to_atom(V, N) when is_float(V) ->
    to_atom(erlang:float_to_binary(V, [{decimals, N}])).

%% about time
to_second(TimeStamp) ->
    if_(TimeStamp > 9999999999, TimeStamp div 1000, TimeStamp).

to_msec(TimeStamp) ->
    to_msec(TimeStamp, 0).

to_msec(TimeStamp, Appen) ->
    if_(TimeStamp > 9999999999, TimeStamp, TimeStamp * 1000 + Appen).

-spec localtime_to_timestamp(DateTime :: calendar:datetime()) -> TimeStamp :: integer().
localtime_to_timestamp({D, T}) ->
    case application:get_env(cool_tools, time_zone, undefined) of
        undefined ->
            DateTimeUTC1 =
                case calendar:local_time_to_universal_time_dst({D, T}) of
                    [DateTimeUTC] ->
                        DateTimeUTC;
                    %% Dst夏令时时间
                    [_DstDateTimeUTC, DateTimeUTC] ->
                        DateTimeUTC
                end,
            S = calendar:datetime_to_gregorian_seconds(DateTimeUTC1),
            S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            S - S1;
        %% 时区+8 -4 等数字
        TimeZone ->
            TimeZoneInt = to_integer(TimeZone),
            %% 当前时间到0年1月1号0点0分0秒的秒数
            SrcSeconds = calendar:datetime_to_gregorian_seconds({D, T}),
            %% 世界时间到0年1月1号0点0分0秒的秒数
            SrcSeconds
            - TimeZoneInt * 3600
            - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    end.

-spec timestamp_to_localtime(TimeStamp :: integer()) -> calendar:datetime().
timestamp_to_localtime(TimeStamp) ->
    MegaSecs = TimeStamp div 1000000,
    Secs = TimeStamp rem 1000000,
    MicroSecs = 0,
    case application:get_env(cool_tools, time_zone, undefined) of
        undefined ->
            calendar:now_to_local_time({MegaSecs, Secs, MicroSecs});
        TimeZone ->
            TimeZoneInt = to_integer(TimeZone),
            calendar:now_to_universal_time({MegaSecs, Secs + TimeZoneInt * 3600, MicroSecs})
    end.

%% 今天凌晨0点的时间
midnight() ->
    {{Y, M, D}, _} = timestamp_to_localtime(timestamp()),
    localtime_to_timestamp({{Y, M, D}, {0, 0, 0}}).

timestamp() ->
    erlang:system_time(1).

epoch() ->
    erlang:system_time(1000).

get_date_short() ->
    TimeStamp = timestamp(),
    to_date_short(TimeStamp).

to_date_short(TimeStamp) when is_integer(TimeStamp) ->
    to_date_short(timestamp_to_localtime(to_second(TimeStamp)));
to_date_short({Date, _}) ->
    to_date(Date, "~p~2..0B~2..0B").

get_date_long() ->
    TimeStamp = timestamp(),
    to_date_long(TimeStamp).

to_date_long(TimeStamp) when is_integer(TimeStamp) ->
    to_date_long(timestamp_to_localtime(to_second(TimeStamp)));
to_date_long({Date, _}) ->
    to_date(Date, "~p-~2..0B-~2..0B").

to_date({Y, M, D}, Format) ->
    lists:flatten(
        io_lib:format(Format, [Y, M, D])).

get_time_short() ->
    to_time_short(timestamp()).

get_time_long() ->
    to_time_long(timestamp()).

to_time_short(TimeStamp) when is_integer(TimeStamp) ->
    {Date, Time} = timestamp_to_localtime(to_second(TimeStamp)),
    to_time_short({Date, Time});
to_time_short({{Y, M, D}, {H, Min, S}}) ->
    lists:flatten(
        io_lib:format("~p~2..0B~2..0B~2..0B~2..0B~2..0B", [Y, M, D, H, Min, S])).

to_time_long(TimeStamp) when is_integer(TimeStamp) ->
    {Date, Time} = timestamp_to_localtime(to_second(TimeStamp)),
    to_time_long({Date, Time});
to_time_long({{Y, M, D}, {H, Min, S}}) ->
    lists:flatten(
        io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S])).

days() ->
    {Ymd, _Hms} = timestamp_to_localtime(timestamp()),
    date_to_days(Ymd).

date_to_days(Date) ->
    {Ymd, _Hms} = timestamp_to_localtime(0),
    erlang:abs(calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Ymd)).

days_to_date(Days) when is_integer(Days) ->
    {Ymd, _} = timestamp_to_localtime(0),
    calendar:gregorian_days_to_date(Days + calendar:date_to_gregorian_days(Ymd)).

time_to_days(Time) when is_integer(Time) ->
    diff_day(to_second(Time), 0).

time_to_hours(Time) when is_integer(Time) ->
    {_, {H, _, _}} = timestamp_to_localtime(to_second(Time)),
    H.

%%距离下一个几时几分还有多少秒
diff_daytime(Hour, Min) ->
    InterVal = Hour * 3600 + Min * 60 - calendar:time_to_seconds(erlang_time()),
    case InterVal >= 0 of
        true ->
            InterVal;
        _ ->
            diff_next_daytime(Hour, Min)
    end.

erlang_localtime() ->
    timestamp_to_localtime(timestamp()).

erlang_time() ->
    {_, Time} = timestamp_to_localtime(timestamp()),
    Time.

%%距离到明天的几时几分还有多少秒
diff_next_daytime(Hour, Min) ->
    86400 + Hour * 3600 + Min * 60 - calendar:time_to_seconds(erlang_time()).

%% 距离下一个小时０分0秒还有多少
diff_next_hour() ->
    diff_next_hour(timestamp()).

diff_next_hour(UnixTimeStamp) when is_integer(UnixTimeStamp) ->
    {_, {_H, M, S}} = timestamp_to_localtime(UnixTimeStamp),
    3600 - M * 60 - S.

diff_day(TimeStamp1, TimeStamp2)
    when erlang:is_integer(TimeStamp1) andalso erlang:is_integer(TimeStamp2) ->
    {Date1, _} = timestamp_to_localtime(TimeStamp1),
    {Date2, _} = timestamp_to_localtime(TimeStamp2),
    diff_day(Date1, Date2);
diff_day(Date1, Date2) ->
    erlang:abs(calendar:date_to_gregorian_days(Date1)
               - calendar:date_to_gregorian_days(Date2)).

get_pre_date_short(LastDayNum) ->
    get_pre_date_short(timestamp(), LastDayNum).

get_pre_date_short(AidDateTime, LastDayNum) ->
    Date = timestamp_to_localtime(AidDateTime - LastDayNum * 86400),
    NewDate = to_date_short(Date),
    {ok, NewDate}.

get_pre_date_long(LastDayNum) ->
    get_pre_date_long(timestamp(), LastDayNum).

get_pre_date_long(AidDateTime, LastDayNum) ->
    Date = timestamp_to_localtime(AidDateTime - LastDayNum * 86400),
    NewDate = to_date_long(Date),
    {ok, NewDate}.

hours() ->
    hours(timestamp()).

hours(Time) ->
    to_second(Time) div 3600.

-spec format_hour(Hour :: integer()) -> list().
format_hour(Hour) ->
    add_time_prefix(Hour).

-spec format_minute(Minute :: integer()) -> list().
format_minute(Minute) ->
    add_time_prefix(Minute).

-spec format_second(Second :: integer()) -> list().
format_second(Second) ->
    add_time_prefix(Second).

-spec add_time_prefix(V :: integer()) -> list().
add_time_prefix(V) when V > 9 ->
    erlang:integer_to_list(V);
add_time_prefix(V) when is_integer(V) ->
    lists:concat([0, V]).

-spec next_month() -> calendar:datetime().
next_month() ->
    next_month(erlang_localtime()).

-spec next_month(LocalTime :: calendar:datetime()) -> calendar:datetime().
next_month({{_Y, _M, D}, _} = LocalTime) ->
    {{Y1, M1, _D1}, Time} = first_day_of_next_month(LocalTime),
    {{Y1, M1, D}, Time}.

-spec first_day_of_this_month() -> calendar:datetime().
first_day_of_this_month() ->
    first_day_of_this_month(erlang_localtime()).

-spec first_day_of_this_month(LocalTime :: calendar:datetime()) -> calendar:datetime().
first_day_of_this_month({{Y, M, _}, Time}) ->
    {{Y, M, 1}, Time}.

-spec first_day_of_next_month() -> calendar:datetime().
first_day_of_next_month() ->
    first_day_of_next_month(erlang_localtime()).

-spec first_day_of_next_month(LocalTime :: calendar:datetime()) -> calendar:datetime().
first_day_of_next_month({{Y, M, _D}, {H, MM, SS}}) ->
    LastDay = calendar:last_day_of_the_month(Y, M),
    T = localtime_to_timestamp({{Y, M, LastDay}, {H, MM, SS}}),
    timestamp_to_localtime(T + 24 * 3600).

%% decode and encode
%% ======================================

sha1(V) when is_integer(V) ->
    sha1(erlang:integer_to_binary(V));
sha1(V) when is_float(V) ->
    sha1(erlang:float_to_binary(V));
sha1(V) when is_atom(V) ->
    sha1(erlang:atom_to_binary(V, utf8));
sha1(V) ->
    to_list(cool_tools_bin:encode_hex(
                crypto:hash(sha, V))).

md5(V) ->
    to_list(cool_tools_bin:encode_hex(
                crypto:hash(md5, V))).

%% @doc 产生小于16的随机字符串
-spec rand_str() -> binary().
rand_str() ->
    B = to_binary(md5(term_to_binary({erlang:system_time(1000), rand:uniform(1000)}))),
    case erlang:size(B) > 16 of
        true ->
            binary:part(B, 0, 16);
        _ ->
            B
    end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
-spec term_to_string(Term :: term()) -> string().
term_to_string(Term) ->
    R = io_lib:format("~p", [Term]),
    lists:flatten(R).

-spec string_to_term(String :: string()) -> {ok, term()}.
string_to_term(String) when is_list(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    Term;
                _Err ->
                    undefined
            end;
        _Error ->
            undefined
    end.

decode_json(Json) ->
    jsx:decode(Json, [return_maps]).

endcode_json(Data) ->
    jsx:encode(Data).

%%
%% 1> Key = hairnet:generate_encoded_key().
encode_token(Term, Key) ->
    hairnet:generate_token(
        erlang:term_to_binary(Term), Key).

-spec decode_token(Token :: binary(), Key :: binary(), Expire :: integer()) ->
                      {ok, Value :: term()} | {error, too_old}.
decode_token(Token, Key, Expire)
    when is_binary(Token), is_binary(Key), is_integer(Expire) ->
    case hairnet:verify_and_decrypt_token(Token, Key, Expire) of
        {ok, V} ->
            {ok, erlang:binary_to_term(V)};
        {error, too_old} ->
            {error, too_old}
    end.

base64_2_safe_url(Base64Bin) ->
    << <<(urlencode_digit(D))>> || <<D>> <= Base64Bin, D =/= $= >>.

safe_url_2_base64(Bin) ->
    Bin2 =
        case byte_size(Bin) rem 4 of
            % 1 -> << Bin/binary, "===" >>;
            2 ->
                <<Bin/binary, "==">>;
            3 ->
                <<Bin/binary, "=">>;
            _ ->
                Bin
        end,
    << <<(urldecode_digit(D))>> || <<D>> <= Bin2 >>.

encode(Bin) when is_binary(Bin) ->
    base64_2_safe_url(base64:encode(Bin));
encode(L) when is_list(L) ->
    encode(iolist_to_binary(L)).

-spec encode_mime(binary() | iolist()) -> binary().
encode_mime(Bin) when is_binary(Bin) ->
    << <<(urlencode_digit(D))>> || <<D>> <= base64:encode(Bin) >>;
encode_mime(L) when is_list(L) ->
    encode_mime(iolist_to_binary(L)).

-spec decode(binary() | iolist()) -> binary().
decode(Bin) when is_binary(Bin) ->
    Bin2 =
        case byte_size(Bin) rem 4 of
            % 1 -> << Bin/binary, "===" >>;
            2 ->
                <<Bin/binary, "==">>;
            3 ->
                <<Bin/binary, "=">>;
            _ ->
                Bin
        end,
    base64:decode(<< <<(urldecode_digit(D))>> || <<D>> <= Bin2 >>);
decode(L) when is_list(L) ->
    decode(iolist_to_binary(L)).

urlencode_digit($/) ->
    $_;
urlencode_digit($+) ->
    $-;
urlencode_digit(D) ->
    D.

urldecode_digit($_) ->
    $/;
urldecode_digit($-) ->
    $+;
urldecode_digit(D) ->
    D.

decode_pagekey(Bin) when is_binary(Bin) ->
    erlang:binary_to_term(
        base64:decode(Bin)).

decode_url_pagekey(Bin) when is_binary(Bin) ->
    decode_pagekey(safe_url_2_base64(Bin)).

encode_pagekey(Time, Key) when is_integer(Time), is_binary(Key) ->
    base64:encode(
        erlang:term_to_binary({Time, Key})).

encode_url_pagekey(Time, Key) ->
    base64_2_safe_url(encode_pagekey(Time, Key)).

%% for
%% if
%% =============================
for(_, Acc, 0) ->
    Acc;
for(F, Acc, N) ->
    NewAcc = F(Acc),
    for(F, NewAcc, N - 1).

if_(Flag, Bool1, Bool2) ->
    case Flag of
        true ->
            Bool1;
        _ ->
            Bool2
    end.

pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> proc_lib:spawn(fun() -> do_f(S, F, I) end) end, L),
    gather(Pids, []).

gather([H | T], Acc) ->
    receive
        {H, Ret} ->
            gather(T, [Ret | Acc])
    end;
gather([], Acc) ->
    Acc.

do_f(Parent, F, I) ->
    Parent ! {self(), catch F(I)}.

%% uuid
%% ====================================
-spec uuid_v1() -> binary().
uuid_v1() ->
    S1 = case get({?MODULE, uuid_v1}) of
             undefined ->
                 uuid:new(self());
             S ->
                 S
         end,
    {UUID, NewS} = uuid:get_v1(S1),
    put({?MODULE, uuid_v1}, NewS),
    UUID.

-spec uuid_v1_string() -> list().
uuid_v1_string() ->
    uuid_to_string(uuid_v1()).

uuid_v1_string(standard) ->
    uuid_to_string(uuid_v1(), standard).

uuid_v1_int() ->
    uuid_to_int(uuid_v1()).

uuid_v4() ->
    uuid:get_v4().

uuid_v4_int() ->
    uuid_to_int(uuid_v4()).

uuid_v4_string() ->
    uuid_to_string(uuid_v4()).

uuid_v4_string(standard) ->
    uuid_to_string(uuid_v4(), standard).

uuid_to_int(U) when is_binary(U) ->
    <<Rs:128/unsigned-integer>> = U,
    Rs.

uuid_to_string(U) ->
    uuid:uuid_to_string(U, nodash).

uuid_to_string(U, standard) ->
    uuid:uuid_to_string(U, standard).

%% other
%% ====================================
-spec list_join(L :: list(), Join :: term()) -> list().
list_join(L, Join) ->
    list_join(L, Join, []).

list_join([], _, Acc) ->
    lists:reverse(Acc);
list_join([H], Join, Acc) ->
    list_join([], Join, [H | Acc]);
list_join([H | Rest], Join, Acc) ->
    list_join(Rest, Join, [Join, H | Acc]).

to_upper(Str) when is_list(Str) ->
    erlang:list_to_binary(
        string:to_upper(Str));
to_upper(Str) when is_binary(Str) ->
    to_upper(erlang:binary_to_list(Str)).

encode_login_password(RealPass, Time) when is_binary(RealPass) ->
    P = to_upper(sha1(RealPass)),
    to_upper(md5(<<P/binary, (erlang:integer_to_binary(Time))/binary>>)).

count_mixed_chars(String) ->
    %%    case re:run(String, <<"\\p{Han}|\\p{Latin}|\\p{Nd}|\\p{P}|\\p{Z}|\\p{Cc}">>, [unicode, global]) of
    case re:run(String, <<"\\p{Han}|\\p{Cc}|.">>, [unicode, global]) of
        {match, Matches} ->
            length(Matches);
        nomatch ->
            0
    end.

count_chinese_chars(String) ->
    case re:run(String, <<"\\p{Han}">>, [unicode, global]) of
        {match, Matches} ->
            length(Matches);
        nomatch ->
            0
    end.

os_cmd(Command) ->
    case os:type() of
        {win32, _} ->
            %% Clink workaround; see
            %% https://code.google.com/p/clink/issues/detail?id=141
            os:cmd(" " ++ Command);
        _ ->
            %% Don't just return "/bin/sh: <cmd>: not found" if not found
            Exec = hd(string:tokens(Command, " ")),
            case os:find_executable(Exec) of
                false ->
                    throw({command_not_found, Exec});
                _ ->
                    os:cmd(Command)
            end
    end.

total_queue_len() ->
    lists:foldl(fun(Pid, MsgQueLen) ->
                   case erlang:process_info(Pid, [message_queue_len]) of
                       [{message_queue_len, Msgs}] ->
                           Msgs + MsgQueLen;
                       _ ->
                           MsgQueLen
                   end
                end,
                0,
                erlang:processes()).

total_process_len() ->
    erlang:length(
        erlang:processes()).
