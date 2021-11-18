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
-module(cool_tools_logger_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

-define(APP, 'cool_tools').
all() ->
    [log].

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

log(_) ->
    ?ERROR_MSG(#{txt => <<"I'am kkkk">>}),
    ?ERROR_MSG(#{term => {<<"I'am kkkk">>, a, #{b=>1}}}),
    ?ERROR_MSG_IF(true,#{txt => <<"I'am error">>}),
    ?TRY_CATCH(throw(11), <<"SUITE">>),
    ok.