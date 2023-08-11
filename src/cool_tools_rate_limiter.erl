%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 6月 2023 16:40
%%%-------------------------------------------------------------------
-module(cool_tools_rate_limiter).

-author("cam").

-define(SECONDS_TO_NANOSECONDS, 1000000000).

%% API
-export([take_token/4, take_token/5, take_token/6, take_token/7, rate_limit/4,
  rate_limit/5, start_link/0, i/0, i/1, timestamp_to_localtime/2, localtime_to_timestamp/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(rate_limit_store, {key :: term(), val = 0, ttl = 0}).
-record(key, {waiting = false, queue = queue:new()}).
-record(state, {keys = #{}}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

i() ->
  gen_server:call(?MODULE, i).

i(queue_len) ->
  gen_server:call(?MODULE, {i, queue_len}).

take_token(Key, Burst, Count, Seconds) ->
  take_token(Key, Burst, Count, Seconds, 1).

take_token(Key, Burst, Count, Seconds, Quantity) ->
  take_token(Key, Burst, Count, Seconds, Quantity, 5000).

take_token(Key, Burst, Count, Seconds, Quantity, MaxQueueCount) ->
  take_token(Key, Burst, Count, Seconds, Quantity, MaxQueueCount, 1000).

-spec take_token(Key :: term(),
    Burst :: pos_integer(),
    Count :: pos_integer(),
    Seconds :: pos_integer(),
    Quantity :: pos_integer(),
    MaxQueueCount :: pos_integer(),
    Timeout :: pos_integer()) ->
  {error, timeout} | boolean().
take_token(Key, Burst, Count, Seconds, Quantity, MaxQueueCount, Timeout) ->
  case catch gen_server:call(?MODULE,
    {take_token,
      {Key, Burst, Count, Seconds, Quantity, ttl(Timeout)},
      MaxQueueCount},
    Timeout)
  of
    {'EXIT', {timeout, _}} ->
      {error, timeout};
    V ->
      V
  end.

ttl(infinity) ->
  infinity;
ttl(Timeout) ->
  erlang:system_time(1000) + Timeout.

rate_limit(Key, Burst, Count, Seconds) ->
  rate_limit(Key, Burst, Count, Seconds, 1).

rate_limit(Key, Burst, Count, Seconds, Quantity) ->
  gen_server:call(?MODULE, {rate_limit, {Key, Burst, Count, Seconds, Quantity}}).

init([]) ->
  process_flag(trap_exit, true),
  init_cache_db(),
  register_midnight_event(init),
  {ok, #state{}}.

init_cache_db() ->
  rate_limit_store =
    ets:new(rate_limit_store,
      [named_table,
        set,
        public,
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #rate_limit_store.key}]),
  ok.

handle_call({i, queue_len}, _From, #state{keys = Keys} = State) ->
  {reply,
    maps:fold(fun(Key, #key{queue = Q} = KeyR, Acc) ->
      Acc#{Key =>
      KeyR#key{queue =
      erlang:length(
        queue:to_list(Q))}}
              end,
      Keys,
      Keys),
    State};
handle_call(i, _From, #state{keys = Keys} = State) ->
  {reply,
    maps:fold(fun(Key, #key{queue = Q} = KeyR, Acc) ->
      Acc#{Key => KeyR#key{queue = queue:to_list(Q)}}
              end,
      Keys,
      Keys),
    State};
handle_call(i, _From, State) ->
  {reply, State, State};
handle_call({take_token,
  {Key, _Burst, _Count, _Seconds, _Quantity, _Time} = Msg,
  MaxQueueCount},
    From,
    #state{keys = Keys} = State) ->
  #key{queue = Queue, waiting = Waiting} = KeyR = maps:get(Key, Keys, #key{}),
  case Waiting of
    true ->
      case queue:len(Queue) > MaxQueueCount of
        true ->
          {reply, false, State};
        _ ->
          {noreply,
            assign(Key, KeyR#key{queue = queue:in({take_token, Msg, From}, Queue)}, State)}
      end;
    _ ->
      case do_rate_limit(filter_msg(Msg)) of
        #{allowed := true} ->
          {reply, true, State};
        #{retry_after := WaitTime} when WaitTime /= 0 ->
          erlang:send_after(WaitTime, self(), {try_again, Msg, From}),
          {noreply, assign(Key, KeyR#key{waiting = true}, State)}
      end
  end;
handle_call({rate_limit, Msg}, _From, State) ->
  {reply, do_rate_limit(Msg), State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({try_again, {Key, _Burst, _Count, _Seconds, _Quantity, TimeOut} = Msg, From},
    #state{keys = Keys} = State) ->
  #key{queue = Queue} = KeyR = maps:get(Key, Keys, #key{}),
  %% timeout will not reply to caller
  case erlang:system_time(1000) > TimeOut of
    true ->
      do_handle_queue_out(Queue, Key, Keys, KeyR, State);
    _ ->
      case do_rate_limit(filter_msg(Msg)) of
        #{allowed := true} ->
          gen:reply(From, true),
          do_handle_queue_out(Queue, Key, Keys, KeyR, State);
        #{retry_after := WaitTime} when WaitTime /= 0 ->
          erlang:send_after(WaitTime, self(), {try_again, Msg, From}),
          {noreply, assign(Key, KeyR#key{waiting = true}, State)}
      end
  end;
handle_info(midnight_event, State) ->
  L = ets:foldl(fun filter/2, [], rate_limit_store),
  lists:foldl(fun del/2, ok, L),
  timer:sleep(10),
  register_midnight_event(),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

do_rate_limit({Key, Burst, Count, Seconds, Quantity})
  when is_integer(Burst), Count > 0, is_integer(Seconds), is_integer(Quantity) ->
  PerPeriod = ?SECONDS_TO_NANOSECONDS * Seconds / Count,
  DelayTolerance = (Burst + 1) * PerPeriod,
  Limit = Burst + 1,
  Tat0 = get_(Key),
  Now = erlang:system_time(nanosecond),
  Incr = Quantity * PerPeriod,
  Tat = calc_tat(Tat0, Now),
  NewTat =
    case Now > Tat of
      true ->
        Now + Incr;
      _ ->
        Tat + Incr
    end,
  AllowAt = NewTat - DelayTolerance,
  Diff = Now - AllowAt,
  {RetryAfter, Limited, TTl} =
    case Diff < 0 of
      true ->
        {retry_after(Incr, DelayTolerance, Diff), true, Tat - Now};
      _ ->
        TTl0 = NewTat - Now,
        set_(Key, NewTat, TTl0),
        {-1, false, TTl0}
    end,
  Next = DelayTolerance - TTl,
  Remaining = remaining(Next, PerPeriod),
  #{allowed => Limited == false,
    limit => Limit,
    remaining => Remaining,
    reset_after => nanosecond_to_mill(TTl),
    retry_after => nanosecond_to_mill(RetryAfter)}.

nanosecond_to_mill(-1) ->
  -1;
nanosecond_to_mill(T) ->
  case trunc(T / 1000000) of
    0 ->
      1;
    V ->
      V
  end.

remaining(Next, PerPeriod) when Next > -PerPeriod ->
  trunc(Next / PerPeriod);
remaining(_, _) ->
  0.

retry_after(Incr, DelayTolerance, Diff) when Incr =< DelayTolerance ->
  -Diff;
retry_after(_, _, _) ->
  -1.

calc_tat(-1, Now) ->
  Now;
calc_tat(Tat, _Now) ->
  Tat.

set_(Key, Val, TTL) ->
  Now = erlang:system_time(nanosecond),
  true =
    ets:insert(rate_limit_store,
      #rate_limit_store{key = Key,
        val = Val,
        ttl = Now + TTL}).

get_(Key) ->
  Now = erlang:system_time(nanosecond),
  case ets:lookup(rate_limit_store, Key) of
    [#rate_limit_store{val = Val, ttl = TTL}] when TTL > Now ->
      Val;
    _ ->
      -1
  end.

register_midnight_event(init) ->
  M = midnight(),
  erlang:send_after((M - erlang:system_time(1)) * 1000, self(), midnight_event).

register_midnight_event() ->
  erlang:send_after(24 * 3600 * 1000, self(), midnight_event).

midnight() ->
  {{Y, M, D}, {_H, _MM, _S}} = timestamp_to_localtime(erlang:system_time(1) + 24 * 3600, 8),
  localtime_to_timestamp({{Y, M, D}, {0, 0, 0}}, 8).

-spec timestamp_to_localtime(TimeStamp :: integer(), TimeZone :: integer() | undefined) ->
  calendar:datetime().
timestamp_to_localtime(TimeStamp, TimeZone) ->
  MegaSecs = TimeStamp div 1000000,
  Secs = TimeStamp rem 1000000,
  MicroSecs = 0,
  case TimeZone of
    undefined ->
      calendar:now_to_local_time({MegaSecs, Secs, MicroSecs});
    TimeZoneInt ->
      calendar:now_to_universal_time({MegaSecs, Secs + TimeZoneInt * 3600, MicroSecs})
  end.

-spec localtime_to_timestamp(DateTime :: calendar:datetime(),
    TimeZone :: integer() | undefined) ->
  TimeStamp :: integer().
localtime_to_timestamp({D, T}, undefined) ->
  DateTimeUTC1 =
    case calendar:local_time_to_universal_time_dst({D, T}) of
      [DateTimeUTC] ->
        DateTimeUTC;
      %% Dst夏令时时间
      [_DstDateTimeUTC, DateTimeUTC] ->
        DateTimeUTC
    end,
  S = calendar:datetime_to_gregorian_seconds(DateTimeUTC1),
  S1 = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  S - S1;
localtime_to_timestamp({D, T}, TimeZoneInt) ->
  %% 当前时间到0年1月1号0点0分0秒的秒数
  SrcSeconds = calendar:datetime_to_gregorian_seconds({D, T}),
  %% 世界时间到0年1月1号0点0分0秒的秒数
  SrcSeconds
    - TimeZoneInt * 3600
    - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

filter(#rate_limit_store{key = Key, ttl = TTL}, Acc) ->
  case erlang:system_time(nanosecond) - TTL > 0 of
    true ->
      [Key | Acc];
    _ ->
      Acc
  end.

del(Key, Acc) ->
  Now = erlang:system_time(nanosecond),
  case ets:lookup(rate_limit_store, Key) of
    [] ->
      Acc;
    [#rate_limit_store{ttl = TTL}] when Now > TTL ->
      true = ets:delete(rate_limit_store, Key),
      Acc;
    _->
      Acc
  end.

assign(Key, KeyR, #state{keys = Keys} = State) ->
  State#state{keys = Keys#{Key => KeyR}}.

filter_msg({Key, Burst, Count, Seconds, Quantity, _Time}) ->
  {Key, Burst, Count, Seconds, Quantity}.

do_handle_queue_out(Queue, Key, Keys, KeyR, State) ->
  case queue:out(Queue) of
    {{value, {_, Msg1, From1}}, NewQueue} ->
      erlang:send(self(), {try_again, Msg1, From1}),
      {noreply, assign(Key, KeyR#key{queue = NewQueue}, State)};
    {empty, _NewQueue} ->
      {noreply, State#state{keys = maps:remove(Key, Keys)}}
  end.
