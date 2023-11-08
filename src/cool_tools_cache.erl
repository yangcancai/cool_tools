%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 11月 2023 10:43
%%%-------------------------------------------------------------------
-module(cool_tools_cache).

-author("cam").

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(Cache, ?MODULE).

-record(cool_tools_cache_state, {ttl_trees = gb_trees:empty()}).

-export([async_set/2, async_set/3, set/2, set/3, get/1, manual_check_ttl/0, clear/0]).

%%%===================================================================
%%% API
%%%===================================================================
async_set(Key, Value) ->
    gen_server:cast(?MODULE, {set, Key, Value}).

async_set(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {set, Key, Value, TTL}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

set(Key, Value, TTL) ->
    gen_server:call(?MODULE, {set, Key, Value, TTL}).

get(Keys) when is_list(Keys) ->
    [ets:lookup(?MODULE, Key) || Key <- Keys];
get(Key) ->
    ets:lookup(?MODULE, Key).

manual_check_ttl() ->
    gen_server:call(?MODULE, manual_check_ttl).

clear() ->
    gen_server:call(?MODULE, clear).

%% @doc Spawns the server and registers the local name (unique)
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: #cool_tools_cache_state{}} |
              {ok, State :: #cool_tools_cache_state{}, timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init([]) ->
    ?Cache = ets:new(?Cache, [named_table, set, public, {read_concurrency, true}]),
    start_tick_timer(),
    {ok, #cool_tools_cache_state{}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: #cool_tools_cache_state{}) ->
                     {reply, Reply :: term(), NewState :: #cool_tools_cache_state{}} |
                     {reply,
                      Reply :: term(),
                      NewState :: #cool_tools_cache_state{},
                      timeout() | hibernate} |
                     {noreply, NewState :: #cool_tools_cache_state{}} |
                     {noreply, NewState :: #cool_tools_cache_state{}, timeout() | hibernate} |
                     {stop,
                      Reason :: term(),
                      Reply :: term(),
                      NewState :: #cool_tools_cache_state{}} |
                     {stop, Reason :: term(), NewState :: #cool_tools_cache_state{}}.
handle_call(clear, _From, State) ->
    true = ets:delete_all_objects(?Cache),
    {reply, ok, State#cool_tools_cache_state{ttl_trees = gb_trees:empty()}};
handle_call(manual_check_ttl,
            _From,
            State = #cool_tools_cache_state{ttl_trees = Trees}) ->
    {reply, ok, State#cool_tools_cache_state{ttl_trees = do_del_expired_values(Trees)}};
handle_call(Call, _From, State = #cool_tools_cache_state{}) ->
    {reply, ok, erlang:element(2, handle_cast(Call, State))}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #cool_tools_cache_state{}) ->
                     {noreply, NewState :: #cool_tools_cache_state{}} |
                     {noreply, NewState :: #cool_tools_cache_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #cool_tools_cache_state{}}.
handle_cast({set, Key, Value}, State = #cool_tools_cache_state{}) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {noreply, State};
handle_cast({set, Key, Value, TTL}, State = #cool_tools_cache_state{ttl_trees = Trees}) ->
    Now = erlang:system_time(1000),
    true = ets:insert(?MODULE, {Key, Value}),
    {noreply,
     State#cool_tools_cache_state{ttl_trees = gb_trees:insert({Now + TTL, Key}, 1, Trees)}};
handle_cast(_Request, State = #cool_tools_cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #cool_tools_cache_state{}) ->
                     {noreply, NewState :: #cool_tools_cache_state{}} |
                     {noreply, NewState :: #cool_tools_cache_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #cool_tools_cache_state{}}.
handle_info(tick, State = #cool_tools_cache_state{ttl_trees = Trees}) ->
    start_tick_timer(),
    {noreply, State#cool_tools_cache_state{ttl_trees = do_del_expired_values(Trees)}};
handle_info(_Info, State = #cool_tools_cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #cool_tools_cache_state{}) ->
                   term().
terminate(_Reason, _State = #cool_tools_cache_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #cool_tools_cache_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #cool_tools_cache_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #cool_tools_cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_tick_timer() ->
    erlang:send_after(1000, self(), tick).

do_del_expired_values(Trees) ->
    do_del_expired_values(Trees, gb_trees:is_empty(Trees)).

do_del_expired_values(Trees, true) ->
    Trees;
do_del_expired_values(Trees, false) ->
    {{Expired, Key}, _} = gb_trees:smallest(Trees),
    case erlang:system_time(1000) > Expired of
        true ->
            true = ets:delete(?Cache, Key),
            do_del_expired_values(gb_trees:delete({Expired, Key}, Trees));
        _ ->
            Trees
    end.
