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

-module(cool_tools_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

all() ->
    [count_chars].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cool_tools),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    application:stop(cool_tools),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    ok.

expect() ->
    ok.

del_meck() ->
    meck:unload().

count_chars(_) ->
    ?assertEqual(0, cool_tools:count_chinese_chars(<<"1234">>)),
    ?assertEqual(0, cool_tools:count_chinese_chars("1234")),
    ?assertEqual(2, cool_tools:count_chinese_chars("1234你好")),
    ?assertEqual(2, cool_tools:count_chinese_chars(<<"1234你好"/utf8>>)),
    ?assertEqual(6, cool_tools:count_mixed_chars("1234你好")),
    ?assertEqual(6, cool_tools:count_mixed_chars(<<"1234你好"/utf8>>)),
    ?assertEqual(8, cool_tools:count_mixed_chars(<<"1234你好,."/utf8>>)),
    ?assertEqual(9, cool_tools:count_mixed_chars(<<"1234你好,.。"/utf8>>)),
    ?assertEqual(11, cool_tools:count_mixed_chars(<<"  1234你好,.。"/utf8>>)),
    ?assertEqual(12, cool_tools:count_mixed_chars(<<"1\n\r1234你好,.。"/utf8>>)),
    ok.
