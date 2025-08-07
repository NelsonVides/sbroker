-module(sregulator_update_meter).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC("""
Updates a list of regulators with the relative time of either queue.

`sregulator_update_meter` can be used as a `sbroker_meter` in a `sbroker` or
a `sregulator`. It will update a list of regulators with the relative time
(in `native` time units) of a specified queue at random intervals, ignoring
any regulators that are not alive. Its argument, `spec()`, is of the form:
```
[{Regulator :: sregulator:regulator(),
  Queue :: ask | ask_r,
  Config :: #{update => Update :: pos_integer()}}, ...].
```
`Regulator` is a regulator process to update with the approximate relative
time of queue `Queue` with updates uniformly distributed from `0.5 * Update`
to `1.5 * Update` milliseconds (defaults to `100`). This random interval is
used to prevent synchronisation of update messages and their side effects,
see reference.

See `m:sregulator`.

References:
Sally Floyd and Van Jacobson, The Synchronization of Periodic
Routing Messages, 1994.
""").

-behaviour(sbroker_meter).

-export([init/2]).
-export([handle_update/5]).
-export([handle_info/3]).
-export([code_change/4]).
-export([config_change/3]).
-export([terminate/2]).

?DOC("Meter specification for updating multiple regulators with queue metrics.").
-type spec() ::
    [
        {
            Regulator :: sregulator:regulator(),
            Queue :: ask | ask_r,
            Config :: #{update => Update :: pos_integer()}
        },
        ...
    ].

-export_type([spec/0]).

-record(entry, {
    regulator :: sregulator:regulator(),
    queue :: ask | ask_r,
    update :: pos_integer(),
    updated = undefined :: undefined | integer()
}).

-record(state, {
    wheel :: gb_trees:tree(integer(), [#entry{}]) | #entry{},
    rand :: rand:state(),
    update_next :: integer()
}).
-type state() :: #state{}.

?DOC(false).
-spec init(Time, Spec | {Spec, Seed}) -> {State, UpdateNext} when
    Time :: integer(),
    Spec :: spec(),
    Seed :: rand:export_state(),
    State :: state(),
    UpdateNext :: integer().
init(Time, {[Regulator], Seed}) ->
    Entry = entry(Regulator),
    Rand = rand:seed_s(Seed),
    {#state{wheel = Entry, rand = Rand, update_next = Time}, Time};
init(Time, {Regulators, Seed}) ->
    Entries = entries(Regulators),
    Wheel = gb_trees:insert(Time, Entries, gb_trees:empty()),
    Rand = rand:seed_s(Seed),
    {#state{wheel = Wheel, rand = Rand, update_next = Time}, Time};
init(Time, Regulators) ->
    Seed = rand:export_seed_s(rand:seed_s(exsplus)),
    init(Time, {Regulators, Seed}).

?DOC(false).
-spec handle_update(QueueDelay, ProcessDelay, RelativeTime, Time, State) ->
    {NState, UpdateNext}
when
    QueueDelay :: non_neg_integer(),
    ProcessDelay :: non_neg_integer(),
    RelativeTime :: integer(),
    Time :: integer(),
    State :: state(),
    NState :: state(),
    UpdateNext :: integer().
handle_update(_, _, _, Time, #state{update_next = UpdateNext} = State) when
    Time < UpdateNext
->
    {State, UpdateNext};
handle_update(
    _,
    _,
    RelativeTime,
    Time,
    #state{
        wheel =
            #entry{
                regulator = Regulator,
                queue = Queue,
                update = Update
            } = Entry,
        rand = Rand
    }
) ->
    cast(Regulator, Queue, RelativeTime),
    NEntry = Entry#entry{updated = Time},
    {Current, NRand} = sbroker_util:uniform_interval_s(Update, Rand),
    Next = Time + Current,
    {#state{wheel = NEntry, rand = NRand, update_next = Next}, Next};
handle_update(_, _, RelativeTime, Time, #state{wheel = Wheel, rand = Rand}) ->
    {_, Entries, NWheel} = gb_trees:take_smallest(Wheel),
    update(Entries, RelativeTime, Time, Rand, NWheel).

?DOC(false).
-spec handle_info(Msg, Time, State) -> {State, UpdateNext} when
    Msg :: term(),
    Time :: integer(),
    State :: state(),
    UpdateNext :: integer().
handle_info(_, Time, State) ->
    handle(Time, State).

?DOC(false).
-spec code_change(OldVsn, Time, State, Extra) -> {State, UpdateNext} when
    OldVsn :: term(),
    Time :: integer(),
    State :: state(),
    Extra :: term(),
    UpdateNext :: integer().
code_change(_, Time, State, _) ->
    handle(Time, State).

?DOC(false).
-spec config_change(Spec | {Spec, Seed}, Time, State) ->
    {NState, UpdateNext}
when
    Time :: integer(),
    Spec :: spec(),
    Seed :: rand:export_state(),
    State :: state(),
    NState :: state(),
    UpdateNext :: integer().
config_change(
    {Regulators, Seed},
    Time,
    #state{wheel = #entry{regulator = Regulator, queue = Queue} = Entry}
) ->
    Rand = rand:seed_s(Seed),
    NEntries = entries(Regulators),
    Old = maps:from_list([{{Regulator, Queue}, Entry}]),
    change(NEntries, Time, Old, Rand);
config_change({Regulators, Seed}, Time, #state{wheel = Wheel}) ->
    Rand = rand:seed_s(Seed),
    NEntries = entries(Regulators),
    OldEntries = [
        {{Regulator, Queue}, Entry}
     || {_, Entries} <- gb_trees:to_list(Wheel),
        Entry = #entry{regulator = Regulator, queue = Queue} <- Entries
    ],
    change(NEntries, Time, maps:from_list(OldEntries), Rand);
config_change(Regulators, Time, #state{rand = Rand} = State) ->
    Seed = rand:export_seed_s(Rand),
    config_change({Regulators, Seed}, Time, State).

?DOC(false).
-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_, _) ->
    ok.

%% Internal

entries(EntriesArg) when length(EntriesArg) > 0 ->
    Entries = [entry(EntryArg) || EntryArg <- EntriesArg],
    DedupList = [
        {{Regulator, Queue}, ignore}
     || #entry{regulator = Regulator, queue = Queue} <- Entries
    ],
    case maps:size(maps:from_list(DedupList)) of
        Size when Size == length(Entries) ->
            Entries;
        _ ->
            error(duplicate_entries, [EntriesArg])
    end;
entries(Other) ->
    error(badarg, [Other]).

entry({Regulator, Queue, Config} = EntryArg) when
    Queue == ask; Queue == ask_r
->
    case is_process(Regulator) of
        true ->
            #entry{
                regulator = Regulator,
                queue = Queue,
                update = sbroker_util:update(Config)
            };
        false ->
            error(badarg, [EntryArg])
    end;
entry(Other) ->
    error(badarg, [Other]).

is_process(Pid) when is_pid(Pid) ->
    true;
is_process(Name) when is_atom(Name) ->
    true;
is_process({Name, Node}) when is_atom(Name), is_atom(Node) ->
    true;
is_process({global, _}) ->
    true;
is_process({via, Mod, _}) when is_atom(Mod) ->
    true;
is_process(_) ->
    false.

update([Entry | Rest], Relative, Time, Rand, Wheel) ->
    #entry{regulator = Regulator, update = Update, queue = Queue} = Entry,
    cast(Regulator, Queue, Relative),
    {Current, NRand} = sbroker_util:uniform_interval_s(Update, Rand),
    NWheel = insert(Time + Current, Entry#entry{updated = Time}, Wheel),
    update(Rest, Relative, Time, NRand, NWheel);
update([], Relative, Time, Rand, Wheel) ->
    case gb_trees:smallest(Wheel) of
        {Next, _} when Next > Time ->
            {#state{wheel = Wheel, rand = Rand, update_next = Next}, Next};
        {Next, Entries} ->
            update(Entries, Relative, Time, Rand, gb_trees:delete(Next, Wheel))
    end.

insert(Next, Entry, Wheel) ->
    % Assume insert unlikely to collide as native time should have higher
    % precision than millisecond and update intervals are randomised
    try gb_trees:insert(Next, [Entry], Wheel) of
        NWheel ->
            NWheel
    catch
        error:_ ->
            Entries = gb_trees:get(Next, Wheel),
            gb_trees:update(Next, [Entry | Entries], Wheel)
    end.

cast(Regulator, ask, RelativeTime) ->
    cast(Regulator, RelativeTime);
cast(Regulator, ask_r, RelativeTime) ->
    cast(Regulator, -RelativeTime).

cast(Regulator, RelativeTime) ->
    try
        sregulator:cast(Regulator, RelativeTime)
    catch
        _:_ ->
            ok
    end.

handle(Time, #state{update_next = UpdateNext} = State) ->
    {State, max(Time, UpdateNext)}.

change([Entry], Time, Old, Rand) ->
    #entry{regulator = Regulator, queue = Queue, update = Update} = Entry,
    case maps:find({Regulator, Queue}, Old) of
        {ok, #entry{updated = Updated}} when is_integer(Updated) ->
            {Current, NRand} = sbroker_util:uniform_interval_s(Update, Rand),
            Next = max(Updated + Current, Time),
            NEntry = Entry#entry{updated = Updated},
            {#state{wheel = NEntry, rand = NRand, update_next = Next}, Next};
        _ ->
            {#state{wheel = Entry, rand = Rand, update_next = Time}, Time}
    end;
change([_ | _] = Entries, Time, Old, Rand) ->
    change(Entries, Time, Old, Rand, gb_trees:empty()).

change([Entry | Entries], Time, Old, Rand, Wheel) ->
    #entry{regulator = Regulator, queue = Queue, update = Update} = Entry,
    case maps:find({Regulator, Queue}, Old) of
        {ok, #entry{updated = Updated}} when is_integer(Updated) ->
            {Current, NRand} = sbroker_util:uniform_interval_s(Update, Rand),
            Next = max(Updated + Current, Time),
            NWheel = insert(Next, Entry#entry{updated = Updated}, Wheel),
            change(Entries, Time, Old, NRand, NWheel);
        _ ->
            NWheel = insert(Time, Entry, Wheel),
            change(Entries, Time, Old, Rand, NWheel)
    end;
change([], _, _, Rand, Wheel) ->
    {Next, _} = gb_trees:smallest(Wheel),
    {#state{wheel = Wheel, rand = Rand, update_next = Next}, Next}.
