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
-module(sregulator_valve_statem).

-include_lib("proper/include/proper.hrl").

-compile({no_auto_import, [time/0]}).

-export([quickcheck/0]).
-export([quickcheck/1]).
-export([check/1]).
-export([check/2]).

-export([initial_state/0]).
-export([command/1]).
-export([precondition/2]).
-export([next_state/3]).
-export([postcondition/3]).

-export([init_or_change/7]).
-export([handle_ask/3]).
-export([handle_done/4]).
-export([handle_continue/4]).
-export([shutdown/5]).

-record(state, {
    manager,
    manager_state,
    mod,
    valve,
    time,
    list = [],
    done = [],
    shutdown = [],
    min,
    max,
    status,
    timeout
}).

quickcheck() ->
    quickcheck([]).

quickcheck(Opts) ->
    proper:quickcheck(prop_sregulator_valve(), Opts).

check(CounterExample) ->
    check(CounterExample, []).

check(CounterExample, Opts) ->
    proper:check(prop_sregulator_valve(), CounterExample, Opts).

prop_sregulator_valve() ->
    ?FORALL(
        Cmds,
        commands(?MODULE),
        ?TRAPEXIT(begin
            {History, State, Result} = run_commands(?MODULE, Cmds),
            sbroker_test_handler:reset(),
            ?WHENFAIL(
                begin
                    io:format("History~n~p", [History]),
                    io:format("State~n~p", [State]),
                    io:format("Result~n~p", [Result])
                end,
                aggregate(command_names(Cmds), Result =:= ok)
            )
        end)
    ).

initial_state() ->
    #state{}.

command(#state{mod = undefined} = State) ->
    {call, ?MODULE, init_or_change, init_or_change_args(State)};
command(#state{mod = Mod} = State) ->
    frequency([
        {handle_update_freq(State), {call, Mod, handle_update, handle_update_args(State)}},
        {12, {call, ?MODULE, handle_ask, handle_ask_args(State)}},
        {8, {call, ?MODULE, handle_done, handle_done_args(State)}},
        {8, {call, ?MODULE, handle_continue, handle_continue_args(State)}},
        {4, {call, ?MODULE, init_or_change, init_or_change_args(State)}},
        {2, {call, ?MODULE, shutdown, shutdown_args(State)}},
        {2, {call, Mod, open_time, open_time_args(State)}},
        {1, {call, Mod, handle_info, handle_info_args(State)}},
        {1, {call, Mod, handle_timeout, handle_timeout_args(State)}},
        {1, {call, Mod, terminate, terminate_args(State)}}
    ]).

precondition(State, {call, _, init_or_change, Args}) ->
    init_or_change_pre(State, Args);
precondition(#state{manager = undefined}, _) ->
    false;
precondition(State, {call, _, handle_update, Args}) ->
    handle_update_pre(State, Args);
precondition(State, {call, _, handle_ask, Args}) ->
    handle_ask_pre(State, Args);
precondition(State, {call, _, handle_done, Args}) ->
    handle_done_pre(State, Args);
precondition(State, {call, _, handle_continue, Args}) ->
    handle_continue_pre(State, Args);
precondition(State, {call, _, handle_info, Args}) ->
    handle_info_pre(State, Args);
precondition(State, {call, _, shutdown, Args}) ->
    shutdown_pre(State, Args);
precondition(State, {call, _, open_time, Args}) ->
    open_time_pre(State, Args);
precondition(State, {call, _, handle_timeout, Args}) ->
    handle_timeout_pre(State, Args);
precondition(State, {call, _, terminate, Args}) ->
    terminate_pre(State, Args).

next_state(State, Value, {call, _, init_or_change, Args}) ->
    init_or_change_next(State, Value, Args);
next_state(State, Value, {call, _, handle_update, Args}) ->
    handle_update_next(State, Value, Args);
next_state(State, Value, {call, _, handle_ask, Args}) ->
    handle_ask_next(State, Value, Args);
next_state(State, Value, {call, _, handle_done, Args}) ->
    handle_done_next(State, Value, Args);
next_state(State, Value, {call, _, handle_continue, Args}) ->
    handle_continue_next(State, Value, Args);
next_state(State, Value, {call, _, handle_info, Args}) ->
    handle_info_next(State, Value, Args);
next_state(State, Value, {call, _, shutdown, Args}) ->
    shutdown_next(State, Value, Args);
next_state(State, Value, {call, _, open_time, Args}) ->
    open_time_next(State, Value, Args);
next_state(State, Value, {call, _, handle_timeout, Args}) ->
    handle_timeout_next(State, Value, Args);
next_state(State, Value, {call, _, terminate, Args}) ->
    terminate_next(State, Value, Args).

postcondition(State, {call, _, init_or_change, Args}, Result) ->
    init_or_change_post(State, Args, Result);
postcondition(State, {call, _, handle_update, Args}, Result) ->
    handle_update_post(State, Args, Result);
postcondition(State, {call, _, handle_ask, Args}, Result) ->
    handle_ask_post(State, Args, Result);
postcondition(State, {call, _, handle_done, Args}, Result) ->
    handle_done_post(State, Args, Result);
postcondition(State, {call, _, handle_continue, Args}, Result) ->
    handle_continue_post(State, Args, Result);
postcondition(State, {call, _, handle_info, Args}, Result) ->
    handle_info_post(State, Args, Result);
postcondition(State, {call, _, shutdown, Args}, Result) ->
    shutdown_post(State, Args, Result);
postcondition(State, {call, _, open_time, Args}, Result) ->
    open_time_post(State, Args, Result);
postcondition(State, {call, _, handle_timeout, Args}, Result) ->
    handle_timeout_post(State, Args, Result);
postcondition(State, {call, _, terminate, Args}, Result) ->
    terminate_post(State, Args, Result).

manager() ->
    frequency([
        {8, sregulator_codel_valve_statem},
        {4, sregulator_rate_valve_statem},
        {4, sregulator_relative_valve_statem},
        {2, sregulator_open_valve_statem},
        {1, sregulator_statem_valve_statem}
    ]).

time() ->
    ?LET(
        Time,
        choose(-10, 10),
        erlang:convert_time_unit(Time, milli_seconds, native)
    ).

time(undefined) ->
    time();
time(Time) ->
    oneof([
        Time,
        ?LET(
            Incr,
            choose(5, 5),
            Time + erlang:convert_time_unit(Incr, milli_seconds, native)
        )
    ]).

relative_time() ->
    Max = erlang:convert_time_unit(10, milli_seconds, native),
    frequency([
        {8, choose(-Max * 3, -Max)},
        {4, choose(-Max, 0)},
        {1, choose(0, Max)}
    ]).

init_or_change(undefined, undefined, undefined, _, Mod, Args, Time) ->
    {Status, State, Timeout} = Mod:init(#{}, Time, Args),
    {ok, Status, State, Timeout};
init_or_change(Mod1, Status1, State1, _, Mod2, Args2, Time) ->
    Callback = {sregulator_valve, Mod1, {Status1, State1}, Mod2, Args2},
    Name = {?MODULE, self()},
    % Should use send time for valve so use time that will crash if used
    case
        sbroker_handlers:config_change(
            Time,
            bad_time,
            [Callback],
            [],
            [],
            Name
        )
    of
        {ok, [{_, _, {NStatus, NState}, Timeout}], {[], infinity}} ->
            {ok, NStatus, NState, Timeout};
        {stop, _} = Stop ->
            Stop
    end.

init_or_change_args(#state{mod = Mod, status = Status, valve = V, time = Time}) ->
    ?LET(
        Manager,
        manager(),
        [
            Mod,
            Status,
            V,
            Manager,
            Manager:module(),
            Manager:args(),
            time(Time)
        ]
    ).

init_or_change_pre(#state{manager = undefined, mod = undefined}, _) ->
    true;
init_or_change_pre(#state{time = PrevTime}, [_, _, _, _, _, _, Time]) ->
    PrevTime >= Time.

init_or_change_next(
    #state{manager = undefined, list = L} = State,
    Value,
    [_, _, _, Manager, Mod, Args, Time]
) ->
    V = {call, erlang, element, [3, Value]},
    Timeout = {call, erlang, element, [4, Value]},
    {Min, Max, Status, ManState} = Manager:init(Args, length(L), Time),
    State#state{
        manager = Manager,
        mod = Mod,
        valve = V,
        time = Time,
        manager_state = ManState,
        status = Status,
        min = Min,
        max = Max,
        timeout = Timeout
    };
init_or_change_next(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState
    } = State,
    Value,
    [Mod, _, _, Manager, Mod, Args, Time]
) ->
    V = {call, erlang, element, [3, Value]},
    Timeout = {call, erlang, element, [4, Value]},
    {Min, Max, NStatus, NManState} = Manager:config_change(
        Args,
        length(L),
        Time,
        ManState
    ),
    State#state{
        valve = V,
        manager_state = NManState,
        time = Time,
        status = NStatus,
        min = Min,
        max = Max,
        timeout = Timeout
    };
init_or_change_next(
    #state{list = L},
    Value,
    [_, _, _, Manager, Mod, Args, Time]
) ->
    InitState = initial_state(),
    init_or_change_next(
        InitState#state{list = L},
        Value,
        [
            undefined,
            undefined,
            undefined,
            Manager,
            Mod,
            Args,
            Time
        ]
    ).

init_or_change_post(#state{manager = undefined}, _, _) ->
    true;
init_or_change_post(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState
    } = State,
    [Mod, _, _, Manager, Mod, Args, Time],
    {ok, Status, V, Timeout}
) ->
    {Min, Max, Status2, NManState} = Manager:config_change(
        Args,
        length(L),
        Time,
        ManState
    ),
    NState = State#state{
        min = Min,
        max = Max,
        manager_state = NManState,
        status = Status2,
        valve = V
    },
    post(NState, Status, Timeout);
init_or_change_post(
    #state{list = L},
    [_, _, _, Manager, Mod, Args, Time],
    {ok, Status, V, Timeout} = Result
) ->
    InitState = initial_state(),
    NState = init_or_change_next(
        InitState#state{list = L},
        Result,
        [
            undefined,
            undefined,
            undefined,
            Manager,
            Mod,
            Args,
            Time
        ]
    ),
    NState2 = NState#state{valve = V},
    post(NState2, Status, Timeout).

handle_update_freq(#state{manager = Manager}) ->
    case erlang:function_exported(Manager, handle_update, 3) of
        true -> 24;
        false -> 1
    end.

handle_update_args(#state{time = Time, valve = V}) ->
    [relative_time(), time(Time), V].

handle_update_pre(#state{time = PrevTime}, [_, Time, _]) ->
    Time >= PrevTime.

handle_update_next(
    #state{manager = Manager, manager_state = ManState} = State,
    Value,
    [RelativeTime, Time, _]
) ->
    V = {call, erlang, element, [2, Value]},
    Timeout = {call, erlang, element, [3, Value]},
    {NStatus, NManState} = handle_update(Manager, RelativeTime, Time, ManState),
    State#state{
        valve = V,
        time = Time,
        manager_state = NManState,
        status = NStatus,
        timeout = Timeout
    }.

handle_update_post(
    #state{manager = Manager, manager_state = ManState} = State,
    [RelativeTime, Time, _],
    {Status, V, Timeout}
) ->
    {Status2, NManState} = handle_update(Manager, RelativeTime, Time, ManState),
    NState = State#state{
        time = Time,
        manager_state = NManState,
        status = Status2,
        valve = V
    },
    post(NState, Status, Timeout).

handle_ask(Mod, Time, V) ->
    {Pid, Ref} = Client = spawn_client(),
    Mod:handle_ask(Pid, Ref, Time, V),
    {go, Open, Status, NV, Timeout} = Mod:handle_ask(Pid, Ref, Time, V),
    {Client, Open, Status, NV, Timeout}.

handle_ask_args(#state{mod = Mod, time = Time, valve = V}) ->
    [Mod, time(Time), V].

handle_ask_pre(
    #state{time = PrevTime, list = L, min = Min, max = Max, status = Status},
    [_, Time, _]
) ->
    Time >= PrevTime andalso
        (length(L) < Min orelse (length(L) < Max andalso Status == open)).

handle_ask_next(
    #state{list = L, manager = Manager, manager_state = ManState} = State,
    Value,
    [_, Time, _]
) ->
    Client = {call, erlang, element, [1, Value]},
    V = {call, erlang, element, [4, Value]},
    Timeout = {call, erlang, element, [5, Value]},
    Handle = ask_function(State),
    {NStatus, NManState} = Manager:Handle(Time, ManState),
    State#state{
        list = L ++ [Client],
        valve = V,
        time = Time,
        manager_state = NManState,
        status = NStatus,
        timeout = Timeout
    }.

handle_ask_post(
    #state{list = L, manager = Manager, manager_state = ManState} = State,
    [_, Time, OldV],
    {Client, OpenTime, Status, V, Timeout}
) ->
    Handle = ask_function(State),
    {Status2, NManState} = Manager:Handle(Time, ManState),
    NState = State#state{
        list = L ++ [Client],
        time = Time,
        manager_state = NManState,
        status = Status2,
        valve = V
    },
    post(NState, Status, Timeout) andalso
        open_time_post(State, [OldV], OpenTime).

handle_done(Mod, undefined, Time, V) ->
    handle_done(Mod, {self(), make_ref()}, Time, V);
handle_done(Mod, {_, Ref}, Time, V) ->
    Mod:handle_done(Ref, Time, V).

handle_done_args(#state{
    mod = Mod,
    list = [],
    done = [],
    shutdown = [],
    time = Time,
    valve = V
}) ->
    [Mod, undefined, time(Time), V];
handle_done_args(#state{
    mod = Mod,
    list = L,
    done = Done,
    shutdown = Shutdown,
    time = Time,
    valve = V
}) ->
    frequency([
        {1, [Mod, undefined, time(Time), V]},
        {16, [Mod, elements(L ++ Done ++ Shutdown), time(Time), V]}
    ]).

handle_done_pre(#state{time = PrevTime}, [_, undefined, Time, _]) ->
    Time >= PrevTime;
handle_done_pre(
    #state{time = PrevTime, list = L, done = Done, shutdown = Shutdown},
    [_, Client, Time, _]
) ->
    Time >= PrevTime andalso
        (lists:member(Client, L) orelse lists:member(Client, Done) orelse
            lists:member(Client, Shutdown)).

handle_done_next(
    #state{
        list = L,
        done = Done,
        manager = Manager,
        manager_state = ManState
    } = State,
    Value,
    [_, Client, Time, _]
) ->
    V = {call, erlang, element, [3, Value]},
    Timeout = {call, erlang, element, [4, Value]},
    NState = State#state{valve = V, time = Time, timeout = Timeout},
    case lists:member(Client, L) of
        true ->
            Handle = done_function(State),
            {NStatus, NManState} = Manager:Handle(Time, ManState),
            NState#state{
                list = L -- [Client],
                done = Done ++ [Client],
                status = NStatus,
                manager_state = NManState
            };
        false ->
            {NStatus, NManState} = Manager:handle(Time, ManState),
            NState#state{status = NStatus, manager_state = NManState}
    end.

handle_done_post(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState
    } = State,
    [_, Client, Time, _],
    {Result, Status, V, Timeout}
) ->
    NState = State#state{valve = V},
    case lists:member(Client, L) of
        true ->
            Handle = done_function(State),
            {Status2, NManState} = Manager:Handle(Time, ManState),
            NState2 = NState#state{
                list = L -- [Client],
                time = Time,
                manager_state = NManState,
                status = Status2
            },
            result_post(Result, done) andalso post(NState2, Status, Timeout);
        false ->
            {Status2, NManState} = Manager:handle(Time, ManState),
            NState2 = State#state{
                time = Time,
                manager_state = NManState,
                status = Status2
            },
            result_post(Result, error) andalso post(NState2, Status, Timeout)
    end.

handle_continue(Mod, undefined, Time, V) ->
    handle_continue(Mod, {self(), make_ref()}, Time, V);
handle_continue(Mod, {_, Ref}, Time, V) ->
    Mod:handle_continue(Ref, Time, V).

handle_continue_args(State) ->
    handle_done_args(State).

handle_continue_pre(State, Args) ->
    handle_done_pre(State, Args).

handle_continue_next(
    #state{
        list = L,
        done = Done,
        manager = Manager,
        manager_state = ManState,
        min = Min,
        max = Max
    } = State,
    Value,
    [_, Client, Time, _]
) ->
    NState = State#state{time = Time},
    case lists:member(Client, L) of
        true when length(L) =< Min ->
            V = {call, erlang, element, [4, Value]},
            Timeout = {call, erlang, element, [5, Value]},
            Handle = done_function(NState),
            {_, NManState} = Manager:Handle(Time, ManState),
            {NStatus2, NManState2} = Manager:handle(Time, NManState),
            NState#state{
                valve = V,
                manager_state = NManState2,
                status = NStatus2,
                timeout = Timeout
            };
        true when length(L) > Max ->
            V = {call, erlang, element, [3, Value]},
            Timeout = {call, erlang, element, [4, Value]},
            Handle = done_function(NState),
            {_, NManState} = Manager:Handle(Time, ManState),
            {NStatus2, NManState2} = Manager:handle(Time, NManState),
            NState#state{
                valve = V,
                list = L -- [Client],
                done = Done ++ [Client],
                manager_state = NManState2,
                status = NStatus2,
                timeout = Timeout
            };
        true ->
            continue_maybe_ask(NState, Value, Client);
        false ->
            V = {call, erlang, element, [3, Value]},
            Timeout = {call, erlang, element, [4, Value]},
            {NStatus, NManState} = Manager:handle(Time, ManState),
            NState#state{
                valve = V,
                manager_state = NManState,
                status = NStatus,
                timeout = Timeout
            }
    end.

handle_continue_post(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState,
        min = Min,
        max = Max
    } = State,
    [_, Client, Time, _],
    {go, _, Status, V, Timeout}
) ->
    Handle = done_function(State),
    {Status2, NManState} = Manager:Handle(Time, ManState),
    NState = State#state{time = Time, status = Status2, valve = V},
    case lists:member(Client, L) of
        true when length(L) =< Min ->
            {NStatus2, NManState2} = Manager:handle(Time, NManState),
            NState2 = NState#state{manager_state = NManState2, status = NStatus2},
            post(NState2, Status, Timeout);
        true when length(L) =< Max, Status2 == open ->
            {NStatus2, NManState2} = Manager:handle_ask(Time, NManState),
            NState2 = NState#state{manager_state = NManState2, status = NStatus2},
            post(NState2, Status, Timeout);
        true ->
            result_post(go, done);
        false ->
            result_post(go, error)
    end;
handle_continue_post(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState,
        max = Max
    } = State,
    [_, Client, Time, _],
    {done, Status, V, Timeout}
) ->
    Handle = done_function(State),
    {NStatus, NManState} = Manager:Handle(Time, ManState),
    NState = State#state{time = Time, valve = V},
    case lists:member(Client, L) of
        true when length(L) > Max; NStatus == closed ->
            {NStatus2, NManState2} = Manager:handle(Time, NManState),
            NState2 = NState#state{
                list = L -- [Client],
                manager_state = NManState2,
                status = NStatus2
            },
            post(NState2, Status, Timeout);
        true ->
            result_post(done, go);
        false ->
            result_post(done, error)
    end;
handle_continue_post(
    #state{
        list = L,
        manager = Manager,
        manager_state = ManState
    } = State,
    [_, Client, Time, _],
    {error, Status, V, Timeout}
) ->
    case lists:member(Client, L) of
        true ->
            result_post(error, done_or_go);
        false ->
            {NStatus, NManState} = Manager:handle(Time, ManState),
            NState = State#state{
                time = Time,
                manager_state = NManState,
                status = NStatus,
                valve = V
            },
            post(NState, Status, Timeout)
    end.

handle_info_args(#state{time = Time, valve = V}) ->
    [oneof([a, b, c]), time(Time), V].

handle_info_pre(#state{time = PrevTime}, [_, Time, _]) ->
    Time >= PrevTime.

handle_info_next(
    #state{manager = Manager, manager_state = ManState} = State,
    Value,
    [_, Time, _]
) ->
    V = {call, erlang, element, [2, Value]},
    Timeout = {call, erlang, element, [3, Value]},
    {Status, NManState} = Manager:handle(Time, ManState),
    State#state{
        manager_state = NManState,
        status = Status,
        time = Time,
        valve = V,
        timeout = Timeout
    }.

handle_info_post(
    #state{manager = Manager, manager_state = ManState} = State,
    [_, Time, _],
    {Status, V, Timeout}
) ->
    {Status2, NManState} = Manager:handle(Time, ManState),
    NState = State#state{
        time = Time,
        manager_state = NManState,
        status = Status2,
        valve = V
    },
    post(NState, Status, Timeout).

handle_timeout_args(#state{time = Time, valve = V}) ->
    [time(Time), V].

handle_timeout_pre(#state{time = PrevTime}, [Time, _]) ->
    Time >= PrevTime.

handle_timeout_next(
    #state{manager = Manager, manager_state = ManState} = State,
    Value,
    [Time, _]
) ->
    V = {call, erlang, element, [2, Value]},
    Timeout = {call, erlang, element, [3, Value]},
    {Status, NManState} = Manager:handle(Time, ManState),
    State#state{
        manager_state = NManState,
        status = Status,
        time = Time,
        valve = V,
        timeout = Timeout
    }.

handle_timeout_post(
    #state{manager = Manager, manager_state = ManState} = State,
    [Time, _],
    {Status, V, Timeout}
) ->
    {Status2, NManState} = Manager:handle(Time, ManState),
    NState = State#state{
        time = Time,
        manager_state = NManState,
        status = Status2,
        valve = V
    },
    post(NState, Status, Timeout).

shutdown_args(#state{
    list = L,
    done = Done,
    shutdown = Shutdown,
    mod = Mod,
    time = Time,
    valve = V
}) ->
    Args = [time(Time), V],
    NoMon = [Mod, oneof([undefined] ++ Done ++ Shutdown), nomonitor | Args],
    case L of
        [] ->
            NoMon;
        _ ->
            Mon = [Mod, oneof(L), monitor | Args],
            frequency([{4, Mon}, {1, NoMon}])
    end.

shutdown(Mod, undefined, nomonitor, Time, V) ->
    shutdown(Mod, {self(), make_ref()}, nomonitor, Time, V);
shutdown(Mod, {Pid, Ref}, nomonitor, Time, V) ->
    Msg = {'DOWN', Ref, process, Pid, shutdown},
    Mod:handle_info(Msg, Time, V);
shutdown(Mod, {Pid, Ref}, monitor, Time, V) ->
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} = Down ->
            Mod:handle_info(Down, Time, V)
    after 5000 ->
        timeout
    end.

shutdown_pre(#state{time = PrevTime, list = L}, [_, Client, monitor, Time, _]) ->
    Time >= PrevTime andalso lists:member(Client, L);
shutdown_pre(
    #state{time = PrevTime, done = Done, shutdown = Shutdown},
    [_, Client, nomonitor, Time, _]
) ->
    Time >= PrevTime andalso
        (lists:member(Client, Done) orelse
            lists:member(Client, Shutdown) orelse Client =:= undefined).

shutdown_next(
    #state{
        list = L,
        shutdown = Shutdown,
        manager = Manager,
        manager_state = ManState
    } = State,
    Value,
    [_, Client, _, Time, _]
) ->
    V = {call, erlang, element, [2, Value]},
    Timeout = {call, erlang, element, [3, Value]},
    NState = State#state{valve = V, time = Time, timeout = Timeout},
    case lists:member(Client, L) of
        true ->
            Handle = done_function(State),
            {NStatus, NManState} = Manager:Handle(Time, ManState),
            NState#state{
                list = L -- [Client],
                shutdown = Shutdown ++ [Client],
                manager_state = NManState,
                status = NStatus
            };
        false ->
            {NStatus, NManState} = Manager:handle(Time, ManState),
            NState#state{manager_state = NManState, status = NStatus}
    end.

shutdown_post(
    #state{list = L, manager = Manager, manager_state = ManState} = State,
    [_, Client, _, Time, _],
    {Status, V, Timeout}
) ->
    NState = State#state{valve = V, time = Time},
    case lists:member(Client, L) of
        true ->
            Handle = done_function(State),
            {NStatus, NManState} = Manager:Handle(Time, ManState),
            NState2 = NState#state{
                list = L -- [Client],
                manager_state = NManState,
                status = NStatus
            },
            post(NState2, Status, Timeout);
        false ->
            {Status2, NManState} = Manager:handle(Time, ManState),
            NState2 = NState#state{manager_state = NManState, status = Status2},
            post(NState2, Status, Timeout)
    end.

open_time_args(#state{valve = E}) ->
    [E].

open_time_pre(_, _) ->
    true.

open_time_next(State, _, _) ->
    State.

open_time_post(
    #state{status = Status, list = L, min = Min, max = Max, time = Time},
    _,
    OpenTime
) when
    length(L) < Min orelse (Status == open andalso length(L) < Max)
->
    case OpenTime =< Time of
        true ->
            true;
        false ->
            ct:pal(
                "Open Time~nExpected: =<~p~nObserved: ~p",
                [Time, OpenTime]
            ),
            false
    end;
open_time_post(#state{time = Time}, _, OpenTime) ->
    case OpenTime of
        closed ->
            true;
        _ when is_integer(OpenTime), OpenTime > Time ->
            true;
        _ ->
            ct:pal(
                "Open Time~nExpected: closed or >~p~nObserved: ~p",
                [Time, OpenTime]
            ),
            false
    end.

terminate_args(#state{valve = V}) ->
    [oneof([shutdown, normal, abnormal]), V].

terminate_pre(_, _) ->
    true.

terminate_next(State, _, _) ->
    State.

terminate_post(#state{list = L}, _, Result) ->
    is_map(Result) andalso
        lists:sort(maps:values(Result)) == lists:sort([Pid || {Pid, _} <- L]).

%% Helpers

post(#state{list = L, min = Min} = State, Status, Timeout) when length(L) < Min ->
    size_post(State) andalso status_post(open, Status) andalso
        timeout_post(State, Timeout);
post(#state{list = L, max = Max} = State, Status, Timeout) when length(L) >= Max ->
    size_post(State) andalso status_post(closed, Status) andalso
        timeout_post(State, Timeout);
post(#state{status = Status2} = State, Status, Timeout) ->
    size_post(State) andalso status_post(Status2, Status) andalso
        timeout_post(State, Timeout).

status_post(Expected, Observed) when Expected =:= Observed ->
    true;
status_post(Expected, Observed) ->
    ct:pal("Status~nExpected: ~p~nObserved: ~p", [Expected, Observed]),
    false.

size_post(#state{mod = Mod, list = L, valve = V}) ->
    Expected = length(L),
    case Mod:size(V) of
        Observed when Observed == Expected ->
            true;
        Observed ->
            ct:pal("Size~nExpected: ~p~nObserved: ~p", [Expected, Observed]),
            false
    end.

result_post(Observed, Expected) when Expected =:= Observed ->
    true;
result_post(Observed, Expected) ->
    ct:pal("Result~nExpected: ~p~nObserved: ~p", [Expected, Observed]),
    false.

timeout_post(#state{list = L, min = Min}, Timeout) when length(L) < Min ->
    do_timeout_post(infinity, Timeout);
timeout_post(#state{list = L, max = Max}, Timeout) when length(L) >= Max ->
    do_timeout_post(infinity, Timeout);
timeout_post(
    #state{manager = Manager, time = Time, manager_state = ManState},
    Timeout
) ->
    case erlang:function_exported(Manager, timeout, 2) of
        true -> do_timeout_post(Manager:timeout(Time, ManState), Timeout);
        false -> do_timeout_post(infinity, Timeout)
    end.

do_timeout_post(Expected, Expected) ->
    true;
do_timeout_post(Expected, Observed) ->
    ct:pal("Timeout~nExpected: ~pObserved: ~p", [Expected, Observed]),
    false.

done_function(#state{list = L, min = Min}) ->
    case length(L) =< Min of
        true -> handle;
        false -> handle_done
    end.

ask_function(#state{list = L, min = Min}) ->
    case length(L) < Min of
        true -> handle;
        false -> handle_ask
    end.

handle_update(Manager, RelativeTime, Time, ManState) ->
    case erlang:function_exported(Manager, handle_update, 3) of
        true -> Manager:handle_update(RelativeTime, Time, ManState);
        false -> Manager:handle(Time, ManState)
    end.

continue_maybe_ask(
    #state{
        time = Time,
        list = L,
        done = Done,
        manager = Manager,
        manager_state = ManState
    } = State,
    Value,
    Client
) ->
    Handle = done_function(State),
    case Manager:Handle(Time, ManState) of
        {open, NManState} ->
            V = {call, erlang, element, [4, Value]},
            Timeout = {call, erlang, element, [5, Value]},
            {NStatus2, NManState2} = Manager:handle_ask(Time, NManState),
            State#state{
                valve = V,
                manager_state = NManState2,
                status = NStatus2,
                timeout = Timeout
            };
        {closed, NManState} ->
            V = {call, erlang, element, [3, Value]},
            Timeout = {call, erlang, element, [4, Value]},
            {NStatus2, NManState2} = Manager:handle(Time, NManState),
            State#state{
                valve = V,
                list = L -- [Client],
                done = Done ++ [Client],
                manager_state = NManState2,
                status = NStatus2,
                timeout = Timeout
            }
    end.

spawn_client() ->
    Parent = self(),
    spawn_monitor(fun() -> client_init(Parent) end).

client_init(Parent) ->
    Ref = monitor(process, Parent),
    receive
        {'DOWN', Ref, _, _, _} ->
            exit(shutdown)
    end.
