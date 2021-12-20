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
%%% Created : 2021-12-20T06:14:03+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_timer).

-author("yangcancai").

-include("cool_tools_logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, add/3, remove/1, get_list/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-type name() :: term().
-type callback() :: {atom(), atom(), list()} | function() | {function(), list()}.
-type interval() ::
    hour | day | {once, pos_integer()} | {pos_integer(), pos_integer()} | pos_integer().

-record(state, {ref = undefined, expired = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_list() ->
    ets:tab2list(?MODULE).

-spec add(name(), Callback :: callback(), interval()) -> ok.
add(Name, Callback, {Min, Max} = AfterInterval)
    when is_integer(Min), is_integer(Max), Max > Min, Min >= 0 ->
    do_add(Name, Callback, AfterInterval);
add(Name, Callback, AfterInterval) when AfterInterval == day; AfterInterval == hour ->
    do_add(Name, Callback, AfterInterval);
add(Name, Callback, {once, I} = AfterInterval) when is_integer(I), I >= 0 ->
    do_add(Name, Callback, AfterInterval);
add(Name, Callback, AfterInterval) when is_integer(AfterInterval), AfterInterval >= 0 ->
    do_add(Name, Callback, AfterInterval).

do_add(Name, {Mod, F, A} = Callback, AfterInterval)
    when is_atom(Mod), is_atom(F), is_list(A) ->
    do_add1(Name, Callback, AfterInterval);
do_add(Name, {F, A} = Callback, AfterInterval) when is_function(F), is_list(A) ->
    do_add1(Name, Callback, AfterInterval);
do_add(Name, F = Callback, AfterInterval) when is_function(F) ->
    do_add1(Name, Callback, AfterInterval).

do_add1(Name, Callback, AfterInterval) ->
    gen_server:call(?MODULE, {add, {Name, Callback, AfterInterval}}).

remove(Name) ->
    gen_server:call(?MODULE,
                    {remove,
                     Name}).%%%===================================================================
                            %%% gen_server callbacks
                            %%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: #state{}} |
              {ok, State :: #state{}, timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, ordered_set, public]),
    erlang:send(self(), go),
    {ok, #state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call({add, {Name, Callback, AfterInterval}},
            _From,
            State = #state{ref = undefined}) ->
  do_remove_timer(Name),
  do_add_timer(Name, Callback, AfterInterval),
    {reply, ok, State};
handle_call({add, {Name, Callback, AfterInterval}}, _From, State = #state{ref = Ref}) ->
    erlang:cancel_timer(Ref),
    do_remove_timer(Name),
    do_add_timer(Name, Callback, AfterInterval),
    NewS = do_go(State#state{ref = undefined, expired = 0}),
    {reply, ok, NewS};
handle_call({remove, Name}, _From, State = #state{ref = undefined}) ->
    do_remove_timer(Name),
    {reply, ok, State};
handle_call({remove, Name}, _, State = #state{ref = Ref}) ->
    erlang:cancel_timer(Ref),
    do_remove_timer(Name),
    NewS = do_go(State#state{ref = undefined, expired = 0}),
    {reply, ok, NewS};
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_info(go, State) ->
    NewState = do_go(State),
    {noreply, NewState};
handle_info({run_task, {ExpiredTime, Name}, Callback, AfterInterval}, State) ->
    do_run_task({ExpiredTime, Name}, Callback, AfterInterval),
    NewState = do_go(State),
    {noreply, NewState};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #state{}) ->
                   term().
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_add_timer(Name, Callback, AfterInterval) ->
    true =
        ets:insert(?MODULE,
                   {{erlang:system_time(1000) + pre_interval(AfterInterval), Name},
                    Callback,
                    AfterInterval}),
    ok.

do_remove_timer(Name) ->
    M = ets:fun2ms(fun ({{_, Name1}, _, _}) when Name == Name1 ->
                           true;
                       (_) ->
                           false
                   end),
    ets:select_delete(?MODULE, M),
    ok.

pre_interval({S, E}) when is_integer(S), is_integer(E) ->
    S + rand:uniform(E - S + 1) - 1;
pre_interval(hour) ->
    cool_tools:diff_next_hour() * 1000;
pre_interval(day) ->
    cool_tools:diff_next_daytime(0, 0) * 1000;
pre_interval({once, I}) ->
    I;
pre_interval(I) ->
    I.

do_del_timer({ExpiredTime, Name}) ->
    true = ets:delete(?MODULE, {ExpiredTime, Name}),
    ok.

do_run_task({ExpiredTime, Name}, Callback, AfterInterval) ->
    spawn(fun() -> do_pre_apply(Callback) end),
    do_del_timer({ExpiredTime, Name}),
    case AfterInterval of
        {once, _} ->
            ok;
        _ ->
            do_add_timer(Name, Callback, AfterInterval)
    end.

do_pre_apply(Callback) ->
    try
        Rs = do_apply(Callback),
        ?LOG_DEBUG("Execute Timer Task, callback = ~p, result = ~p", [Callback, Rs]),
        Rs
    catch
        E:R:Stack ->
            ?LOG_ERROR("Execute Timer Task, callback = ~p, result = ~p", [Callback, {E, R, Stack}])
    end.

do_apply({M, F, A}) ->
    apply(M, F, A);
do_apply({Fun, A}) when is_function(Fun) ->
    apply(Fun, A);
do_apply(Fun) when is_function(Fun) ->
    Fun().

do_go(State) ->
    case ets:first(?MODULE) of
        '$end_of_table' ->
            erlang:send_after(1000, self(), go),
            State#state{ref = undefined, expired = 0};
        {ExpiredTime, _Name} = Key ->
            [{_, Callback, AfterInterval}] = ets:lookup(?MODULE, Key),
            case ExpiredTime - erlang:system_time(1000) of
                After when After > 0 ->
                    Ref = erlang:send_after(After,
                                            self(),
                                            {run_task, Key, Callback, AfterInterval}),
                    State#state{ref = Ref, expired = AfterInterval};
                _ ->
                    Ref = erlang:send(self(), {run_task, Key, Callback, AfterInterval}),
                    State#state{ref = Ref}
            end
    end.
