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

-export([sha1/1, md5/1, to_upper/1, encode_login_password/2]).

sha1(V) when is_integer(V) ->
    sha1(erlang:integer_to_binary(V));
sha1(V) when is_float(V) ->
    sha1(erlang:float_to_binary(V));
sha1(V) when is_atom(V) ->
    sha1(erlang:atom_to_binary(V, utf8));
sha1(V) ->
    bin2hex(crypto:hash(sha, V)).

md5(V) ->
    bin2hex(crypto:hash(md5, V)).

bin2hex(B) ->
    L = binary_to_list(B),
    LH0 = lists:map(fun(X) -> integer_to_list(X, 16) end, L),
    LH = lists:map(fun ([X, Y]) ->
                           [X, Y];
                       ([X]) ->
                           [$0, X]
                   end,
                   LH0), % add zeros
    lists:flatten(LH).

to_upper(Str) when is_list(Str) ->
    erlang:list_to_binary(
        string:to_upper(Str));
to_upper(Str) when is_binary(Str) ->
    to_upper(erlang:binary_to_list(Str)).

encode_login_password(RealPass, Time) when is_binary(RealPass) ->
    P = to_upper(sha1(RealPass)),
    to_upper(md5(<<P/binary, (erlang:integer_to_binary(Time))/binary>>)).
