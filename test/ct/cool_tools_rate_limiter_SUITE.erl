%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 6月 2023 16:41
%%%-------------------------------------------------------------------
-module(cool_tools_rate_limiter_SUITE).

-include("cool_tools_ct.hrl").

-compile(export_all).

all() ->
    [rate_limit, take_token].

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

rate_limit(_) ->
    #{allowed := true} = cool_tools_rate_limiter:rate_limit(a, 1, 1, 1),
    #{allowed := true} = cool_tools_rate_limiter:rate_limit(b, 0, 1, 1),
    #{allowed := false} = cool_tools_rate_limiter:rate_limit(b, 0, 1, 1),
    ok.

take_token(_) ->
    ?assertEqual(true, cool_tools_rate_limiter:take_token(c, 0, 1, 10)),
    ?assertEqual({error, timeout}, cool_tools_rate_limiter:take_token(c, 0, 1, 10)),
    receive
        _V ->
            ?assertMatch(ok, {error, not_expected_to_receive})
    after 2000 ->
        ok
    end,
    %% burst + 1 = Max
    [true = cool_tools_rate_limiter:take_token(d, 1000, 1, 2) || _ <- lists:seq(1, 1001)],
    %% default timeout equal 1000ms
    {error, timeout} = cool_tools_rate_limiter:take_token(d, 1000, 1, 2),
    %% already wait 2000ms
    true = cool_tools_rate_limiter:take_token(d, 1000, 1, 2),
    ok.
