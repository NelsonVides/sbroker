-module(sregulator_underload_meter).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Sets an alarm when the regulator`s valve is slow to get a match for an
interval.

`sregulator_underload_meter` can be used in as a `sbroker_meter` in a
`sregulator`. It will set a SASL `alarm_handler` alarm when the regulator`s
valve is slow to get a match for an interval and clear it once the valve
gets matches fast for an interval. Its argument, `spec()`, is of the form:
```
#{alarm    => Alarm :: term(), % default: {underload, self()}
  target   => Target :: integer(), % default: 100
  interval => Interval :: pos_integer()}. % default: 1000
```
`Alarm` is the `alarm_handler` alarm that will be set/cleared by the meter
(defaults to `{underload, self()}`). `Target` is the target relative time for
the valve to get a match (defaults to `100`). `Interval` is the interval in
milliseconds (defaults to `1000`) that the valve must be above the target for
the alarm to be set and below the target for the alarm to be cleared. The
description of the alarm is `{valve_slow, self()}`.

This meter is only intended for a `sregulator` because a `sregulator_valve`
will remain open when processes that should be available are not sending
requests and can not do anything to correct this. Whereas a `sbroker` can
not distinguish between one queue receiving too many requests or the other
too few. In either situation the congested `sbroker_queue` would drop
requests to correct the imbalance, causing a congestion alarm to be cleared
very quickly.
""").

-behaviour(sbroker_meter).

-export([init/2]).
-export([handle_update/5]).
-export([handle_info/3]).
-export([code_change/4]).
-export([config_change/3]).
-export([terminate/2]).

%% types

-type spec() ::
    #{
        alarm => Alarm :: term(),
        target => Target :: integer(),
        interval => Interval :: pos_integer()
    }.

-record(state, {
    target :: integer(),
    interval :: pos_integer(),
    alarm_id :: term(),
    status = clear :: clear | set,
    toggle_next = infinity :: integer() | infinity
}).

-type state() :: #state{}.

?DOC(false).
-spec init(Time, Spec) -> {State, infinity} when
    Time :: integer(),
    Spec :: spec(),
    State :: state().
init(_, Spec) ->
    AlarmId = sbroker_util:alarm(underload, Spec),
    Target = sbroker_util:relative_target(Spec),
    Interval = sbroker_util:interval(Spec),
    alarm_handler:clear_alarm(AlarmId),
    {#state{target = Target, interval = Interval, alarm_id = AlarmId}, infinity}.

?DOC(false).
-spec handle_update(QueueDelay, ProcessDelay, RelativeTime, Time, State) ->
    {NState, Next}
when
    QueueDelay :: non_neg_integer(),
    ProcessDelay :: non_neg_integer(),
    RelativeTime :: integer(),
    Time :: integer(),
    State :: state(),
    NState :: state(),
    Next :: integer() | infinity.
handle_update(
    _,
    _,
    RelativeTime,
    Time,
    #state{
        status = clear,
        target = Target,
        interval = Interval,
        alarm_id = AlarmId,
        toggle_next = ToggleNext
    } = State
) ->
    case {-RelativeTime < Target, ToggleNext, ToggleNext > Time} of
        {true, infinity, _} ->
            {State, ToggleNext};
        {true, _, _} ->
            {State#state{toggle_next = infinity}, infinity};
        {false, infinity, _} ->
            NToggleNext = Time + Interval,
            {State#state{toggle_next = NToggleNext}, NToggleNext};
        {false, _, true} ->
            {State, ToggleNext};
        _ ->
            alarm_handler:set_alarm({AlarmId, {valve_slow, self()}}),
            {State#state{status = set, toggle_next = infinity}, infinity}
    end;
handle_update(
    _,
    _,
    RelativeTime,
    Time,
    #state{
        status = set,
        target = Target,
        interval = Interval,
        alarm_id = AlarmId,
        toggle_next = ToggleNext
    } = State
) ->
    case {-RelativeTime < Target, ToggleNext, ToggleNext > Time} of
        {true, infinity, _} ->
            NToggleNext = Time + Interval,
            {State#state{toggle_next = NToggleNext}, NToggleNext};
        {true, _, true} ->
            {State, ToggleNext};
        {true, _, _} ->
            alarm_handler:clear_alarm(AlarmId),
            {State#state{status = clear, toggle_next = infinity}, infinity};
        {false, infinity, _} ->
            {State, infinity};
        _ ->
            {State#state{toggle_next = infinity}, infinity}
    end.

?DOC(false).
-spec handle_info(Msg, Time, State) -> {State, Next} when
    Msg :: term(),
    Time :: integer(),
    State :: state(),
    Next :: integer() | infinity.
handle_info(_, Time, #state{toggle_next = ToggleNext} = State) ->
    {State, max(Time, ToggleNext)}.

?DOC(false).
-spec code_change(OldVsn, Time, State, Extra) -> {NState, Next} when
    OldVsn :: term(),
    Time :: integer(),
    State :: state(),
    Extra :: term(),
    NState :: state(),
    Next :: integer() | infinity.
code_change(_, Time, #state{toggle_next = ToggleNext} = State, _) ->
    {State, max(Time, ToggleNext)}.

?DOC(false).
-spec config_change(Spec, Time, State) -> {NState, Next} when
    Spec :: spec(),
    Time :: integer(),
    State :: state(),
    NState :: state(),
    Next :: integer() | infinity.
config_change(
    Spec,
    Time,
    #state{
        alarm_id = AlarmId,
        interval = Interval,
        status = Status,
        toggle_next = ToggleNext
    } = State
) ->
    case sbroker_util:alarm(underload, Spec) of
        AlarmId when ToggleNext == infinity ->
            NTarget = sbroker_util:relative_target(Spec),
            NInterval = sbroker_util:interval(Spec),
            {State#state{target = NTarget, interval = NInterval}, infinity};
        AlarmId when is_integer(ToggleNext) ->
            NTarget = sbroker_util:relative_target(Spec),
            NInterval = sbroker_util:interval(Spec),
            NToggleNext = ToggleNext + NInterval - Interval,
            NState = State#state{
                target = NTarget,
                interval = NInterval,
                toggle_next = NToggleNext
            },
            {NState, max(Time, NToggleNext)};
        _ when Status == set ->
            alarm_handler:clear_alarm(AlarmId),
            init(Time, Spec);
        _ when Status == clear ->
            init(Time, Spec)
    end.

?DOC(false).
-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_, #state{status = set, alarm_id = AlarmId}) ->
    alarm_handler:clear_alarm(AlarmId);
terminate(_, _) ->
    ok.
