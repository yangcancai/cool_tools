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

-module(cool_tools_sup).

-author("yangcancai").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 10},
    ChildSpecs =
        [#{id => cool_tools_timer,
           start => {cool_tools_timer, start_link, []},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [cool_tools_timer]},
         #{id => cool_tools_rate_limiter,
           start => {cool_tools_rate_limiter, start_link, []},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [cool_tools_rate_limiter]},
         #{id => cool_tools_cache,
           start => {cool_tools_cache, start_link, []},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [cool_tools_cache]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
