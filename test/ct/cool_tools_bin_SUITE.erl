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
%%% Created : 2021-09-17T01:46:53+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_bin_SUITE).

-author("yangcancai").

-include("cool_tools_ct.hrl").

-compile(export_all).

-define(APP, cool_tools).

all() ->
    [hex_encoding].

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
    % ok = meck:new(cool_tools_bin, [non_strict, no_link]),
    ok.

expect() ->
    % ok = meck:expect(cool_tools_bin, test, fun() -> {ok, 1} end).
    ok.

del_meck() ->
    meck:unload().

hex_encoding(_Config) ->
    %% Vector test imported from the RFC 4648 section 10.
    <<>> = cool_tools_bin:encode_hex(<<>>),
    <<"66">> = cool_tools_bin:encode_hex(<<"f">>),
    <<"666F">> = cool_tools_bin:encode_hex(<<"fo">>),
    <<"666F6F">> = cool_tools_bin:encode_hex(<<"foo">>),
    <<"666F6F62">> = cool_tools_bin:encode_hex(<<"foob">>),
    <<"666F6F6261">> = cool_tools_bin:encode_hex(<<"fooba">>),
    <<"666F6F626172">> = cool_tools_bin:encode_hex(<<"foobar">>),

    <<>> = cool_tools_bin:decode_hex(<<>>),
    <<"f">> = cool_tools_bin:decode_hex(<<"66">>),
    <<"fo">> = cool_tools_bin:decode_hex(<<"666F">>),
    <<"foo">> = cool_tools_bin:decode_hex(<<"666F6F">>),
    <<"foob">> = cool_tools_bin:decode_hex(<<"666F6F62">>),
    <<"fooba">> = cool_tools_bin:decode_hex(<<"666F6F6261">>),
    <<"foobar">> = cool_tools_bin:decode_hex(<<"666F6F626172">>),

    <<"fo">> = cool_tools_bin:decode_hex(<<"666f">>),
    <<"foo">> = cool_tools_bin:decode_hex(<<"666f6f">>),
    <<"foob">> = cool_tools_bin:decode_hex(<<"666f6f62">>),
    <<"fooba">> = cool_tools_bin:decode_hex(<<"666f6f6261">>),
    <<"foobar">> = cool_tools_bin:decode_hex(<<"666f6f626172">>),

    <<"foobar">> = cool_tools_bin:decode_hex(<<"666f6F626172">>),
    ok.
