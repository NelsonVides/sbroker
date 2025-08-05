%%-------------------------------------------------------------------
%%
%% Copyright (c) 2015, James Fish <james@fishcakez.com>
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
-module(sbroker_overload_statem).

-include_lib("proper/include/proper.hrl").

-export([module/0]).
-export([args/0]).
-export([init/2]).
-export([update_next/6]).
-export([update_post/6]).
-export([change/3]).
-export([timeout/2]).

-record(state, {target, interval, alarm, time, status, queue, interval_time}).

module() ->
    sbroker_overload_meter.

args() ->
    ?LET(
        {Target, Interval, Alarm},
        {choose(0, 3), choose(1, 5), desc()},
        #{
            target => Target,
            interval => Interval,
            alarm => Alarm
        }
    ).

desc() ->
    oneof([a, b, c]).

init(Time, #{target := Target, interval := Interval, alarm := Alarm}) ->
    State = #state{
        target = erlang:convert_time_unit(
            Target,
            milli_seconds,
            native
        ),
        interval = erlang:convert_time_unit(
            Interval,
            milli_seconds,
            native
        ),
        alarm = Alarm,
        status = fast,
        time = Time,
        queue = fast,
        interval_time = 0
    },
    {State, timeout(State, Time)}.

update_next(State, Time, MsgQLen, QueueDelay, _, _) ->
    NState = next(queue(MsgQLen, QueueDelay, State), State, Time),
    {NState, timeout(NState, Time)}.

update_post(State, Time, MsgQLen, QueueDelay, _, _) ->
    NState = next(queue(MsgQLen, QueueDelay, State), State, Time),
    {alarm_post(NState), timeout(NState, Time)}.

change(
    #state{alarm = Alarm, interval_time = IntervalTime, time = PrevTime} = State,
    Time,
    #{target := Target, interval := Interval, alarm := Alarm}
) ->
    NIntervalTime = IntervalTime - PrevTime + Time,
    NState = State#state{
        target = erlang:convert_time_unit(
            Target,
            milli_seconds,
            native
        ),
        interval = erlang:convert_time_unit(
            Interval,
            milli_seconds,
            native
        ),
        time = Time,
        interval_time = NIntervalTime
    },
    {NState, timeout(NState, Time)};
change(_, Time, Args) ->
    init(Time, Args).

timeout(#state{status = Status, queue = Status}, _) ->
    infinity;
timeout(
    #state{
        time = PrevTime,
        interval_time = IntervalTime,
        interval = Interval
    },
    Time
) ->
    NIntervalTime = IntervalTime - PrevTime + Time,
    Time + max(0, Interval - NIntervalTime).

%% Internal

queue(_, QueueDelay, #state{target = Target}) when QueueDelay < Target ->
    fast;
queue(0, 0, #state{target = 0}) ->
    fast;
queue(_, _, _) ->
    slow.

next(Status, #state{status = Status, queue = Status} = State, Time) ->
    State#state{time = Time};
next(
    NStatus,
    #state{
        queue = NStatus,
        interval = Interval,
        time = PrevTime,
        interval_time = IntervalTime
    } = State,
    Time
) ->
    case IntervalTime - PrevTime + Time of
        NIntervalTime when NIntervalTime >= Interval ->
            State#state{status = NStatus, interval_time = 0, time = Time};
        NIntervalTime ->
            State#state{interval_time = NIntervalTime, time = Time}
    end;
next(NStatus, #state{} = State, Time) ->
    State#state{queue = NStatus, interval_time = 0, time = Time}.

alarm_post(#state{status = fast, alarm = Alarm}) ->
    case sbroker_test_handler:get_alarms() of
        [] ->
            true;
        [{Alarm, {message_queue_slow, Pid}}] when Pid == self() ->
            ct:pal("Alarm set"),
            false
    end;
alarm_post(#state{status = slow, alarm = Alarm}) ->
    case sbroker_test_handler:get_alarms() of
        [] ->
            ct:pal("Alarm clear"),
            false;
        [{Alarm, {message_queue_slow, Pid}}] when Pid == self() ->
            true
    end.
