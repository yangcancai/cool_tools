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
%%% Created : 2021-09-08T09:52:16+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_backend_rdbms_mysql).

-author("yangcancai").

-behaviour(cool_tools_backend_rdbms).

-export([init/1, start/0, call/0]).

init(A) ->
    {mysql_init, A}.

start() ->
    mysql_start.

call() ->
    mysql_call.
