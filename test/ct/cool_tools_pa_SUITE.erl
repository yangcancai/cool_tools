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
%%% Created : 2021-11-18T07:25:32+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_pa_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

-define(APP, cool_tools).

all() ->
    [pa].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(?APP),
    cool_tools_logger:set_global_loglevel(all),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    ok = application:stop(?APP),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    % ok = meck:new(cool_tools_logger, [non_strict, no_link]),
    ok.

expect() ->
    % ok = meck:expect(cool_tools_logger, test, fun() -> {ok, 1} end).
    ok.

del_meck() ->
    meck:unload().

pa(_) ->
     ?assertEqual(do_something_on_a_list_of_items([a,b]), [{a, 1,2,3}, {b, 1,2,3}]),
     ok.


do_something_on_a_list_of_items(ListOfItems) ->
    SomeVar1 = 1,
    SomeVar2 = 2,
    SomeVar3 = 3,
    lists:reverse(lists:foldl(cool_tools_pa:bind(fun do_something_with_one_item/5,
                      SomeVar1, SomeVar2, SomeVar3), [], ListOfItems)).

do_something_with_one_item(SomeVar1, SomeVar2, SomeVar3, Elem, Acc) ->
    [{Elem, SomeVar1, SomeVar2, SomeVar3} | Acc].
