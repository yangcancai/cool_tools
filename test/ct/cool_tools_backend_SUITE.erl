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
%%% Created : 2021-09-08T09:50:04+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_backend_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

-define(APP, cool_tools).

all() ->
    [create_backend].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(?APP),
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
    % ok = meck:new(cool_tools_backend, [non_strict, no_link]),
    ok.

expect() ->
    % ok = meck:expect(cool_tools_backend, test, fun() -> {ok, 1} end).
    ok.

del_meck() ->
    meck:unload().%

create_backend(_) ->
    {ok, cool_tools_backend_rdbms_backend} =
        cool_tools_backend:create(cool_tools_backend_rdbms, mysql, []),
    mysql = cool_tools_backend_rdbms_backend:backend_name(),
    cool_tools_backend_rdbms_mysql = cool_tools_backend_rdbms_backend:backend(),
    {mysql_init, a} = cool_tools_backend_rdbms_backend:init(a),

    {ok, cool_tools_backend_rdbms_backend} =
        cool_tools_backend:create(cool_tools_backend_rdbms, pgsql, []),

    {pgsql_init, a} = cool_tools_backend_rdbms_backend:init(a),

    ok.
