%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 7月 2023 12:17
%%%-------------------------------------------------------------------
-module(cool_tools_vm_monitor).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_QUEUE_LEN_CHECK_INTERVAL, 1000).

-record(opts, {queue_len_limit = 0, alarm_funs}).
-record(state,
        {total_queue_len = 0,
         max_queue_len_pid = undefined,
         timeout,
         timer,
         alarmed = false,
         opts}).

-type opts() :: #opts{}.

-export([set_queue_limit/1, set_check_interval/1, set_opts/1, get_total_queue_len/0]).

%%%===================================================================
%%% API
%%%===================================================================
set_queue_limit(Limit) ->
    gen_server:call(?MODULE, {set_queue_limit, Limit}).

set_opts(Opts) ->
    gen_server:call(?MODULE, {set_opts, Opts}).

set_check_interval(Interval) ->
    gen_server:call(?MODULE, {set_check_interval, Interval}).

get_total_queue_len() ->
    cool_tools:total_queue_len().

start_link() ->
    start_link(#opts{queue_len_limit = 50,
                     alarm_funs =
                         {fun alarm_handler:set_alarm/1, fun alarm_handler:clear_alarm/1}}).

%% @doc Spawns the server and registers the local name (unique)
-spec start_link(opts() | integer()) ->
                    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(QueueLenLimit) when is_integer(QueueLenLimit) ->
    start_link(#opts{queue_len_limit = QueueLenLimit,
                     alarm_funs =
                         {fun alarm_handler:set_alarm/1, fun alarm_handler:clear_alarm/1}});
start_link(#opts{} = Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: #state{}} |
              {ok, State :: #state{}, timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init([Opts]) ->
    TRef = erlang:send_after(?DEFAULT_QUEUE_LEN_CHECK_INTERVAL, self(), update),
    {ok,
     #state{opts = Opts,
            timer = TRef,
            timeout = ?DEFAULT_QUEUE_LEN_CHECK_INTERVAL}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call({set_check_interval, Interval}, _, State) when Interval > 0 ->
    {reply, ok, State#state{timeout = Interval}};
handle_call({set_queue_limit, Limit}, _, #state{opts = Opts} = State) when Limit > 0 ->
    {reply, ok, State#state{opts = Opts#opts{queue_len_limit = Limit}}};
handle_call({set_opts,
             #opts{queue_len_limit = Limit, alarm_funs = {SetAlarm, ClearAlarm}} = Opts},
            _,
            State)
    when Limit > 0, is_function(SetAlarm), is_function(ClearAlarm) ->
    {reply, ok, State#state{opts = Opts}}.

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
handle_info(update, #state{timer = OldRef} = State) ->
    erlang:cancel_timer(OldRef),
    S = internal_update(State),
    TRef = erlang:send_after(State#state.timeout, self(), update),
    {noreply, S#state{timer = TRef}};
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
internal_update(#state{opts =
                           #opts{queue_len_limit = Limit, alarm_funs = {SetAlarm, ClearAlarm}},
                       alarmed = Alarmed} =
                    S) ->
    TotalQueueLen = cool_tools:total_queue_len(),
    case {Alarmed, TotalQueueLen > Limit} of
        {false, true} ->
            SetAlarm({{resource_limit, queue_len, node()}, []}),
            S#state{alarmed = true, total_queue_len = TotalQueueLen};
        {true, false} ->
            ClearAlarm({resource_limit, queue_len, node()}),
            S#state{alarmed = false, total_queue_len = TotalQueueLen};
        _ ->
            S#state{total_queue_len = TotalQueueLen}
    end.
