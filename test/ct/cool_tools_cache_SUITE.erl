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

-module(cool_tools_cache_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

all() ->
    [set_and_get, del].

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

set_and_get(_) ->
    %% check
    ?assertEqual([], cool_tools_cache:get(key1)),
    %% call
    ok = cool_tools_cache:set(key1, <<"hello">>),
    ?assertEqual([{key1, <<"hello">>}], cool_tools_cache:get(key1)),
    %% async
    ok = cool_tools_cache:async_set(key1, <<"hello1">>),
    timer:sleep(10),
    ?assertEqual([{key1, <<"hello1">>}], cool_tools_cache:get(key1)),
    %%  ttl
    ok = cool_tools_cache:set(key1, <<"hello2">>, 1),
    ok = cool_tools_cache:set(key2, <<"hello22">>, 2),
    ?assertEqual([{key1, <<"hello2">>}], cool_tools_cache:get(key1)),
    ?assertEqual([{key2, <<"hello22">>}], cool_tools_cache:get(key2)),
    timer:sleep(2),
    cool_tools_cache:manual_check_ttl(),
    ?assertEqual([], cool_tools_cache:get(key1)),
    ?assertEqual([], cool_tools_cache:get(key2)),

    %% clear
    ok = cool_tools_cache:set(key1, <<"hello3">>, 10000),
    ?assertEqual([{key1, <<"hello3">>}], cool_tools_cache:get(key1)),
    cool_tools_cache:clear(),
    ?assertEqual([], cool_tools_cache:get(key1)),
    ok.

del(_) ->
    ?assertEqual([], cool_tools_cache:get(key1)),
    ok = cool_tools_cache:set(key1, <<"hello3">>, 10000),
    ?assertEqual([{key1, <<"hello3">>}], cool_tools_cache:get(key1)),
    ok = cool_tools_cache:set(key1, <<"hello4">>, 1000),
    ?assertEqual([{key1, <<"hello4">>}], cool_tools_cache:get(key1)),
    timer:sleep(2000),
    ?assertEqual([], cool_tools_cache:get(key1)),
    ok = cool_tools_cache:set(key1, <<"hello3">>, 500),
    ?assertEqual([{key1, <<"hello3">>}], cool_tools_cache:get(key1)),
    ok = cool_tools_cache:set(key1, <<"hello4">>, 10000),
    ?assertEqual([{key1, <<"hello4">>}], cool_tools_cache:get(key1)),
    timer:sleep(2000),
    ?assertEqual([{key1, <<"hello4">>}], cool_tools_cache:get(key1)),
    ok = cool_tools_cache:set(key1, <<"hello4">>, 1000),
    cool_tools_cache:del(key1),
    ?assertEqual([], cool_tools_cache:get(key1)),
    ok.
