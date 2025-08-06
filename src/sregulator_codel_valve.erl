%%-------------------------------------------------------------------
%%
%% Copyright (c) 2016, James Fish <james@fishcakez.com>
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%-------------------------------------------------------------------
-module(sregulator_codel_valve).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Implements a valve which increases its size based on CoDel (Controlling
Queue Delay).

`sregulator_codel_valve` can be used as the `sregulator_valve` in a
`sregulator`. It will provide a valve that increases in size in decreasing
intervals while updates remain below a target (based on CoDel) between the
minimum and maximum capacity. Its argument, `spec()`, is of the form:
```
#{target   => Target :: integer(), % default: 100
  interval => Interval :: pos_integer(), % default: 1000
  min      => Min :: non_neg_integer(), % default: 0
  max      => Max :: non_neg_integer() | infinity} % default: infinity
```
`Target` is the target relative value in milliseconds. `Interval` is the
initial interval in milliseconds. The valve will open when updates remain
below the target (in `native` time units) for an interval. Each consecutive
interval is smaller than the last until an update above the target is
received. The valve is always open when below the minimum and always closed
once it reaches the maximum.

This valve tries to enforce a minimum level of concurrency and will grow
while a relevant `sbroker_queue` is moving quickly - up to a maximum.
Therefore this valves expects the updates to be from a
`sregulator_update_meter`.

The algorithm used in this valve is similar to `sbroker_codel_queue`, except
designed to keep the relevant queue slow (but not too slow) instead of fast.
Therefore trying to ensure the counter party to the queue is always fast
without using too many resources. This works by increasing the concurrency or
number of requests when the queue is consistently fast and remaining static
when the queue is slow. Therefore forcing the queue to be slightly slow.

This valve is designed to be used a `sbroker_codel_queue` with the same
`Interval` and `Target`s that are between 10% and 20% of the `Interval`. The
target range is suggested due to the observation in the CoDel paper that the
queue goes from to fast to slow over this target range. Higher targets result
in heavily congested queues and wasted resources. A suggested initial
`Interval` is the 95% percentile of the time it takes to stop a task and
restart, as this is equivalent to the round trip when a packet is dropped and
resent to rejoin the queue in the paper.

Decreasing the target of the valve makes it more resistant to bursts and
reducing the target of the queue will increase the rate of shrinking when
load decreases. This fulfils the general desire to increase resouce usage as
late as possible and decrease resource usage as early as possible. If the
queue`s target is significantly lower than valve`s this may lead to churn as
the queue and valve may act against each other. Also if the minimum is too
high the queue may drop the requests only for the valve to allow immediate
enqueues.

More investigation needs to be done on suitable parameters.

References:
Kathleen Nichols and Van Jacobson, Controlling Queue Delay,
ACM Queue, 6th May 2012.
""").

-behaviour(sregulator_valve).

%% sregulator_valve_api

-export([init/3]).
-export([handle_ask/4]).
-export([handle_done/3]).
-export([handle_continue/3]).
-export([handle_update/3]).
-export([handle_info/3]).
-export([handle_timeout/2]).
-export([code_change/4]).
-export([config_change/3]).
-export([size/1]).
-export([open_time/1]).
-export([terminate/2]).

%% types

-type spec() ::
    #{
        target => Target :: integer(),
        interval => Interval :: pos_integer(),
        min => Min :: non_neg_integer(),
        max => Max :: non_neg_integer() | infinity
    }.

-export_type([spec/0]).

-record(state, {
    min :: non_neg_integer(),
    max :: non_neg_integer() | infinity,
    target :: integer(),
    interval :: pos_integer(),
    count = 0 :: non_neg_integer(),
    open_next :: integer(),
    open_first = infinity :: integer() | infinity | opening | await,
    small_time :: integer(),
    map :: sregulator_valve:internal_map()
}).

-type state() :: #state{}.

%% sregulator_valve api

?DOC(false).
-spec init(Map, Time, Spec) -> {open | closed, State, infinity} when
    Map :: sregulator_valve:internal_map(),
    Time :: integer(),
    Spec :: spec(),
    State :: state().
init(Map, Time, Spec) ->
    {Min, Max} = sbroker_util:min_max(Spec),
    State = #state{
        min = Min,
        max = Max,
        target = sbroker_util:relative_target(Spec),
        interval = sbroker_util:interval(Spec),
        open_next = Time,
        small_time = Time,
        map = Map
    },
    handle(Time, State).

?DOC(false).
-spec handle_ask(Pid, Ref, Time, State) ->
    {go, Open, open | closed, NState, infinity}
when
    Pid :: pid(),
    Ref :: reference(),
    Time :: integer(),
    State :: state(),
    Open :: integer(),
    NState :: state().
handle_ask(
    Pid,
    Ref,
    Time,
    #state{
        min = Min,
        open_first = First,
        open_next = Next,
        small_time = Small,
        count = C,
        map = Map
    } = State
) ->
    NMap = maps:put(Ref, Pid, Map),
    NState = State#state{map = NMap},
    case {map_size(NMap), First, Time >= Next, Time >= First} of
        {Size, _, _, _} when Size < Min ->
            {go, Small, open, NState, infinity};
        %% open based on size of Min-1
        {Min, _, _, _} ->
            go(Small, Time, NState);
        %% opening and fast for a consecutive interval
        {_, opening, true, _} ->
            go(Next, Time, open_control(C + 1, Next, NState));
        %% fast for an initial interval
        {_, _, _, true} ->
            go(First, Time, open_control(Time, NState))
    end.

?DOC(false).
-spec handle_done(Ref, Time, State) ->
    {done | error, open | closed, NState, infinity}
when
    Ref :: reference(),
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_done(Ref, Time, #state{map = Map} = State) ->
    Before = map_size(Map),
    done(Ref, Map, Before, Time, State).

?DOC(false).
-spec handle_continue(Ref, Time, State) ->
    {go, Open, open | closed, NState, infinity}
    | {done | error, open | closed, NState, infinity}
when
    Ref :: reference(),
    Time :: integer(),
    State :: state(),
    Open :: integer(),
    NState :: state().
handle_continue(
    Ref,
    Time,
    #state{
        min = Min,
        max = Max,
        open_first = First,
        open_next = Next,
        small_time = Small,
        count = C,
        map = Map
    } = State
) ->
    Size = map_size(Map),
    case {Size, First, Time >= Next, Time >= First} of
        {S, _, _, _} when S < Min ->
            continue(Ref, Map, Size, Small, Time, State, State);
        {Min, _, _, _} ->
            continue(Ref, Map, Size, Time, Time, State, State);
        {S, _, _, _} when S > Max ->
            done(Ref, Map, Size, Time, State);
        {_, opening, true, _} ->
            NState = open_control(C + 1, Next, State),
            continue(Ref, Map, Size, Next, Time, State, NState);
        {_, F, _, true} when is_integer(F) ->
            NState = open_control(Time, State),
            continue(Ref, Map, Size, First, Time, State, NState);
        _ ->
            done(Ref, Map, Size, Time, State)
    end.

?DOC(false).
-spec handle_update(Value, Time, State) ->
    {open | closed, NState, infinity}
when
    Value :: integer(),
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_update(
    RelativeTime,
    Time,
    #state{
        open_first = infinity,
        target = Target,
        interval = Interval
    } = State
) when RelativeTime < Target ->
    handle(Time, State#state{open_first = Time + Interval});
handle_update(
    RelativeTime,
    Time,
    #state{target = Target, open_first = await} = State
) when
    RelativeTime < Target
->
    handle(Time, State#state{open_first = opening});
handle_update(RelativeTime, Time, #state{target = Target} = State) when
    RelativeTime < Target
->
    handle(Time, State);
handle_update(_, Time, #state{open_first = infinity} = State) ->
    handle(Time, State);
handle_update(_, Time, State) ->
    handle(Time, State#state{open_first = infinity}).

?DOC(false).
-spec handle_info(Msg, Time, State) -> {open | closed, NState, infinity} when
    Msg :: term(),
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_info({'DOWN', Ref, _, _, _}, Time, #state{map = Map, min = Min} = State) ->
    Before = map_size(Map),
    NMap = maps:remove(Ref, Map),
    case map_size(NMap) of
        After when Before =:= Min, After < Min ->
            handle(Time, State#state{map = NMap, small_time = Time});
        _ ->
            handle(Time, State#state{map = NMap})
    end;
handle_info(_, Time, State) ->
    handle(Time, State).

?DOC(false).
-spec handle_timeout(Time, State) -> {open | closed, NState, infinity} when
    Time :: integer(),
    State :: state(),
    NState :: state().
handle_timeout(Time, State) ->
    handle(Time, State).

?DOC(false).
-spec code_change(OldVsn, Time, State, Extra) -> {Status, NState, infinity} when
    OldVsn :: term(),
    Time :: integer(),
    State :: state(),
    Extra :: term(),
    Status :: open | closed,
    NState :: state().
code_change(_, Time, State, _) ->
    handle(Time, State).

?DOC(false).
-spec config_change(Spec, Time, State) -> {open | closed, NState, infinity} when
    Spec :: spec(),
    Time :: integer(),
    State :: state(),
    NState :: state().
config_change(Spec, Time, State) ->
    {Min, Max} = sbroker_util:min_max(Spec),
    NState = State#state{
        min = Min,
        max = Max,
        target = sbroker_util:relative_target(Spec),
        interval = sbroker_util:interval(Spec)
    },
    change(Time, NState).

?DOC(false).
-spec size(State) -> Size when
    State :: state(),
    Size :: non_neg_integer().
size(#state{map = Map}) ->
    map_size(Map).

?DOC(false).
-spec open_time(State) -> Open | closed when
    State :: state(),
    Open :: integer().
open_time(#state{map = Map, min = Min, small_time = Small}) when
    map_size(Map) < Min
->
    Small;
open_time(#state{map = Map, max = Max}) when map_size(Map) >= Max ->
    closed;
open_time(#state{open_first = infinity}) ->
    closed;
open_time(#state{open_first = await}) ->
    closed;
open_time(#state{open_first = opening, open_next = Next}) ->
    Next;
open_time(#state{open_first = First}) ->
    First.

?DOC(false).
-spec terminate(Reason, State) -> Map when
    Reason :: term(),
    State :: state(),
    Map :: sregulator_valve:internal_map().
terminate(_, #state{map = Map}) ->
    Map.

%% Internal

go(Open, Time, #state{map = Map} = State) ->
    {go, Open, status(map_size(Map), Time, State), State, infinity}.

handle(Time, #state{map = Map} = State) ->
    {status(map_size(Map), Time, State), State, infinity}.

status(Size, _, #state{min = Min}) when Size < Min ->
    open;
status(Size, _, #state{max = Max}) when Size >= Max ->
    closed;
status(_, _, #state{open_first = infinity}) ->
    closed;
status(_, _, #state{open_first = await}) ->
    closed;
status(_, Time, #state{open_first = opening, open_next = Next}) when Time < Next ->
    closed;
status(_, _, #state{open_first = opening}) ->
    open;
status(_, Time, #state{open_first = First}) when Time < First ->
    closed;
status(_, _, _) ->
    open.

%% If first fast update in fast interval was "soon" after switching from
%% opening to closed use the previous dropping interval length as it
%% should be appropriate.
open_control(Time, #state{interval = Interval, count = C, open_next = Next} = State) when
    C > 2 andalso Time - Next < 8 * Interval
->
    open_control(C - 2, Time, State);
open_control(Time, #state{interval = Interval} = State) ->
    State#state{count = 1, open_next = Time + Interval, open_first = await}.

%% Shrink the interval to increase open rate and reduce relative time.
open_control(C, Time, #state{interval = Interval} = State) ->
    Next = Time + trunc(Interval / math:sqrt(C)),
    State#state{count = C, open_first = await, open_next = Next}.

continue(Ref, Map, Size, Open, Time, ErrorState, OKState) ->
    case maps:find(Ref, Map) of
        {ok, _} ->
            {go, Open, status(Size, Time, OKState), OKState, infinity};
        error ->
            {error, status(Size, Time, ErrorState), ErrorState, infinity}
    end.

done(Ref, Map, Before, Time, #state{min = Min} = State) ->
    NMap = maps:remove(Ref, Map),
    NState = State#state{map = NMap},
    case map_size(NMap) of
        Before ->
            {error, status(Before, Time, NState), NState, infinity};
        _ when Before =:= Min ->
            {done, open, NState#state{small_time = Time}, infinity};
        After ->
            demonitor(Ref, [flush]),
            {done, status(After, Time, NState), NState, infinity}
    end.

change(Time, #state{open_first = First, interval = Interval} = State) when
    is_integer(First), First > Time + Interval
->
    change(Time, State#state{open_first = Time + Interval});
change(Time, #state{open_next = Next, interval = Interval} = State) when
    is_integer(Next) andalso Next > Time + Interval
->
    change(Time, State#state{open_next = Time + Interval});
change(Time, State) ->
    handle(Time, State).
