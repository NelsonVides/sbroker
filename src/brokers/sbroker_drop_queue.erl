-module(sbroker_drop_queue).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Implements a head or tail drop queue.

`sbroker_drop_queue` can be used as a `sbroker_queue` module in a `sbroker`
or `sregulator`. It will provide a FIFO or LIFO queue that drops the head or
tail request from the queue when a maximum size is exceeded. Its argument,
`spec()`, is of the form:
```
#{out  => Out :: out | out_r, % default: out
  drop => Drop :: drop | drop_r, % default: drop_r
  max  => Max :: non_neg_integer() | infinity} % default: infinity
```
`Out` is either `out` for a FIFO queue (the default) or `out_r` for a LIFO
queue. `Drop` is either `drop_r` for tail drop (the default) where the last
request is droppped, or `drop` for head drop, where the first request is
dropped. Dropping occurs when queue is above the maximum size `Max`
(defaults to `infinity`).
""").

-behaviour(sbroker_queue).
-behaviour(sbroker_fair_queue).

-export([init/3]).
-export([handle_in/5]).
-export([handle_out/2]).
-export([handle_fq_out/2]).
-export([handle_timeout/2]).
-export([handle_cancel/3]).
-export([handle_info/3]).
-export([code_change/4]).
-export([config_change/3]).
-export([len/1]).
-export([send_time/1]).
-export([terminate/2]).

?DOC("Queue specification with drop behavior parameters for queue size control.").
-type spec() ::
    #{
        out => Out :: out | out_r,
        drop => Drop :: drop | drop_r,
        max => Max :: non_neg_integer() | infinity
    }.

-export_type([spec/0]).

-record(state, {
    out :: out | out_r,
    drop :: drop | drop_r,
    max :: non_neg_integer() | infinity,
    len :: non_neg_integer(),
    queue :: sbroker_queue:internal_queue()
}).

-type state() :: #state{}.

?DOC(false).
-spec init(Q, Time, Spec) -> {State, infinity} when
    Q :: sbroker_queue:internal_queue(),
    Time :: integer(),
    Spec :: spec(),
    State :: state().
init(Q, Time, Arg) ->
    from_queue(Q, queue:len(Q), Time, Arg).

?DOC(false).
-spec handle_in(SendTime, From, Value, Time, State) -> {NState, infinity} when
    SendTime :: integer(),
    From :: {pid(), term()},
    Value :: term(),
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_in(
    SendTime,
    From,
    _,
    Time,
    #state{max = Max, len = Max, drop = drop_r} = State
) ->
    sbroker_queue:drop(From, SendTime, Time),
    {State, infinity};
handle_in(
    SendTime,
    {Pid, _} = From,
    Value,
    Time,
    #state{max = Max, len = Max, drop = drop, queue = Q} = State
) ->
    {{value, {SendTime2, From2, _, Ref2}}, NQ} = queue:out(Q),
    erlang:demonitor(Ref2, [flush]),
    sbroker_queue:drop(From2, SendTime2, Time),
    Ref = erlang:monitor(process, Pid),
    NQ2 = queue:in({SendTime, From, Value, Ref}, NQ),
    {State#state{queue = NQ2}, infinity};
handle_in(
    SendTime,
    {Pid, _} = From,
    Value,
    _,
    #state{len = Len, queue = Q} = State
) ->
    Ref = erlang:monitor(process, Pid),
    NQ = queue:in({SendTime, From, Value, Ref}, Q),
    {State#state{len = Len + 1, queue = NQ}, infinity}.

?DOC(false).
-spec handle_out(Time, State) ->
    {SendTime, From, Value, Ref, NState, infinity} | {empty, NState}
when
    Time :: integer(),
    State :: state(),
    SendTime :: integer(),
    From :: {pid(), term()},
    Value :: term(),
    Ref :: reference(),
    NState :: state().
handle_out(_Time, #state{len = 0} = State) ->
    {empty, State};
handle_out(_, #state{out = out, len = Len, queue = Q} = State) ->
    {{value, {SendTime, From, Value, Ref}}, NQ} = queue:out(Q),
    {SendTime, From, Value, Ref, State#state{len = Len - 1, queue = NQ}, infinity};
handle_out(_, #state{out = out_r, len = Len, queue = Q} = State) ->
    {{value, {SendTime, From, Value, Ref}}, NQ} = queue:out_r(Q),
    {SendTime, From, Value, Ref, State#state{len = Len - 1, queue = NQ}, infinity}.

?DOC(false).
-spec handle_fq_out(Time, State) ->
    {SendTime, From, Value, Ref, NState, NextTimeout}
    | {empty, NState, RemoveTime}
when
    Time :: integer(),
    State :: state(),
    SendTime :: integer(),
    From :: {pid(), term()},
    Value :: term(),
    Ref :: reference(),
    NState :: state(),
    NextTimeout :: integer() | infinity,
    RemoveTime :: integer().
handle_fq_out(Time, State) ->
    case handle_out(Time, State) of
        {_, _, _, _, _, _} = Out ->
            Out;
        {empty, NState} ->
            {empty, NState, Time}
    end.

?DOC(false).
-spec handle_cancel(Tag, Time, State) -> {Cancelled, NState, infinity} when
    Tag :: term(),
    Time :: integer(),
    State :: state(),
    Cancelled :: false | pos_integer(),
    NState :: state().
handle_cancel(Tag, _, #state{len = Len, queue = Q} = State) ->
    Cancel = fun
        ({_, {_, Tag2}, _, Ref}) when Tag2 =:= Tag ->
            erlang:demonitor(Ref, [flush]),
            false;
        (_) ->
            true
    end,
    NQ = queue:filter(Cancel, Q),
    case queue:len(NQ) of
        Len ->
            {false, State, infinity};
        NLen ->
            {Len - NLen, State#state{len = NLen, queue = NQ}, infinity}
    end.

?DOC(false).
-spec handle_timeout(Time, State) -> {State, infinity} when
    Time :: integer(),
    State :: state().
handle_timeout(_Time, State) ->
    {State, infinity}.

?DOC(false).
-spec handle_info(Msg, Time, State) -> {NState, infinity} when
    Msg :: term(),
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_info({'DOWN', Ref, _, _, _}, _, #state{queue = Q} = State) ->
    NQ = queue:filter(fun({_, _, _, Ref2}) -> Ref2 =/= Ref end, Q),
    {State#state{len = queue:len(NQ), queue = NQ}, infinity};
handle_info(_, _, State) ->
    {State, infinity}.

?DOC(false).
-spec code_change(OldVsn, Time, State, Extra) -> {NState, infinity} when
    OldVsn :: term(),
    Time :: integer(),
    State :: state(),
    Extra :: term(),
    NState :: state().
code_change(_, _, State, _) ->
    {State, infinity}.

?DOC(false).
-spec config_change(Spec, Time, State) -> {NState, infinity} when
    Spec :: spec(),
    Time :: integer(),
    State :: state(),
    NState :: state().
config_change(Spec, Time, #state{len = Len, queue = Q}) ->
    from_queue(Q, Len, Time, Spec).

?DOC(false).
-spec len(State) -> Len when
    State :: state(),
    Len :: non_neg_integer().
len(#state{len = Len}) ->
    Len.

?DOC(false).
-spec send_time(State) -> SendTime | empty when
    State :: state(),
    SendTime :: integer().
send_time(#state{len = 0}) ->
    empty;
send_time(#state{queue = Q}) ->
    {SendTime, _, _, _} = queue:get(Q),
    SendTime.

?DOC(false).
-spec terminate(Reason, State) -> Q when
    Reason :: term(),
    State :: state(),
    Q :: sbroker_queue:internal_queue().
terminate(_, #state{queue = Q}) ->
    Q.

%% Internal

from_queue(Q, Len, Time, Spec) ->
    Out = sbroker_util:out(Spec),
    Drop = sbroker_util:drop(Spec),
    Max = sbroker_util:max(Spec),
    from_queue(Q, Len, Time, Out, Drop, Max).

from_queue(Q, Len, _, Out, Drop, infinity) ->
    {#state{out = Out, drop = Drop, max = infinity, len = Len, queue = Q}, infinity};
from_queue(Q, Len, Time, Out, Drop, Max) ->
    case Len - Max of
        DropCount when DropCount > 0 andalso Drop =:= drop ->
            {DropQ, NQ} = queue:split(DropCount, Q),
            drop_queue(Time, DropQ),
            NState = #state{out = Out, drop = Drop, max = Max, len = Max, queue = NQ},
            {NState, infinity};
        DropCount when DropCount > 0 andalso Drop =:= drop_r ->
            {NQ, DropQ} = queue:split(Max, Q),
            drop_queue(Time, DropQ),
            NState = #state{out = Out, drop = Drop, max = Max, len = Max, queue = NQ},
            {NState, infinity};
        _ ->
            {#state{out = Out, drop = Drop, max = Max, len = Len, queue = Q}, infinity}
    end.

drop_queue(Time, Q) ->
    _ = [drop_item(Time, Item) || Item <- queue:to_list(Q)],
    ok.

drop_item(Time, {SendTime, From, _, Ref}) ->
    erlang:demonitor(Ref, [flush]),
    sbroker_queue:drop(From, SendTime, Time).
