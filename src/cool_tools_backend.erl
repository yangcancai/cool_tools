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
%%% Created : 2021-09-08T09:25:12+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_backend).

-author("yangcancai").

-export([create/2, create/3]).
-export([backend_module/2]).

%% Callback implemented by proxy modules.
-callback backend() -> module().

%% API

-spec create(For :: module(), Name :: atom()) -> {ok, module()} | {error, already_loaded}.
create(For, Name) ->
    create(For, Name, []).

-spec create(For :: module(), Name :: atom(), TrackedFuns :: [atom()]) ->
                {ok, module()} | {error, already_loaded}.
create(Module, Backend, TrackedFuns) ->
    ProxyModule = proxy_module(Module),
    BackendModule = backend_module(Module, Backend),
    ensure_backend_metrics(Module, TrackedFuns),
    case catch ProxyModule:backend() of
        BackendModule ->
            {error, already_loaded};
        _ ->
            {ProxyModuleStr, CodeString} = backend_code(Module, Backend, TrackedFuns),
            {Mod, Code} = cool_tools_compile:from_string(CodeString),
            code:load_binary(Mod, ProxyModuleStr ++ ".erl", Code),
            {ok, ProxyModule}
    end.

%% Internal functions

-spec proxy_module(Module :: module()) -> module().
proxy_module(Module) ->
    list_to_atom(atom_to_list(Module) ++ "_backend").

-spec backend_module(Module :: module(), Backend :: atom()) -> module().
backend_module(Module, Backend) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Backend)).

-spec backend_code(module(), atom(), list()) -> {nonempty_string(), list()}.
backend_code(Module, Backend, TrackedFuns) when is_atom(Backend) ->
    Callbacks = Module:behaviour_info(callbacks),
    ModuleStr = atom_to_list(Module),
    ProxyModuleName = ModuleStr ++ "_backend",
    RealBackendModule = ModuleStr ++ "_" ++ atom_to_list(Backend),
    BehaviourExports = [generate_export(F, A) || {F, A} <- Callbacks],

    BehaviourImpl =
        [generate_fun(Module, RealBackendModule, F, A, TrackedFuns) || {F, A} <- Callbacks],
    Code =
        lists:flatten(["-module(",
                       ProxyModuleName,
                       ").\n",
                       "-behaviour(backend_module).\n-export([backend/0, backend_name/0]).\n",
                       BehaviourExports,
                       "-spec backend() -> atom().\n",
                       "backend() ->",
                       RealBackendModule,
                       ".\n",
                       "backend_name() ->",
                       atom_to_list(Backend),
                       ".\n",
                       BehaviourImpl]),
    {ProxyModuleName, Code}.

generate_export(F, A) ->
    "-export([" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ "]).\n".

generate_fun(BaseModule, RealBackendModule, F, A, TrackedFuns) ->
    Args = string:join(["A" ++ integer_to_list(I) || I <- lists:seq(1, A)], ", "),
    IsTracked = lists:member(F, TrackedFuns),
    [fun_header(F, Args),
     " ->\n",
     generate_fun_body(IsTracked, BaseModule, RealBackendModule, F, Args)].

fun_header(F, Args) ->
    [atom_to_list(F), "(", Args, ")"].

time_metric(Module, Op) ->
    [backends, Module, Op].

calls_metric(Module, Op) ->
    [backends, Module, calls, Op].

generate_fun_body(false, _, RealBackendModule, F, Args) ->
    ["    ", RealBackendModule, ":", fun_header(F, Args), ".\n"];
generate_fun_body(true, BaseModule, RealBackendModule, F, Args) ->
    FS = atom_to_list(F),
    %%     returned is the following
    %%     cool_tools_metrics:update(global, calls_metric(Backend, F), 1),
    %%     {Time, Result} = timer:tc(Backend, F, Args),
    %%     cool_tools_metrics:update(global, time_metric(Backend, F), Time),
    %%     Result.
    CallsMetric = io_lib:format("~p", [calls_metric(BaseModule, F)]),
    TimeMetric = io_lib:format("~p", [time_metric(BaseModule, F)]),
    ["    cool_tools_metrics:update(global, ",
     CallsMetric,
     ", 1), \n",
     "    {Time, Result} = timer:tc(",
     RealBackendModule,
     ", ",
     FS,
     ", [",
     Args,
     "]), \n",
     "    cool_tools_metrics:update(global, ",
     TimeMetric,
     ", Time), \n",
     "    Result.\n"].

ensure_backend_metrics(Module, Ops) ->
    EnsureFun =
        fun(Op) ->
           cool_tools_metrics:ensure_metric(global, calls_metric(Module, Op), spiral),
           cool_tools_metrics:ensure_metric(global, time_metric(Module, Op), histogram)
        end,
    lists:foreach(EnsureFun, Ops).
