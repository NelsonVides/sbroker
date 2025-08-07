-module(sbroker_meter).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Behaviour for implementing meters for `sbroker` and `sregulator`.

A custom meter must implement the `sbroker_meter` behaviour. The first
callback is `init/2`, which starts the meter:
```
-callback init(Time :: integer(), Args :: term()) ->
     {State :: term(), UpdateTime :: integer() | infinity}.
```
`Time` is the time, in `native` time units, of the meter at creation. Some
other callbacks will receive the current time of the meter as the second last
argument. It is monotically increasing, so subsequent calls will have the
same or a greater time.

`Args` is the arguments for the meter. It can be any term.

`State` is the state of the meter and used in the next call.

`UpdateTime` represents the next time a meter wishes to call
`handle_update/4` to update itself. If a message is not recevued the update
should occur at or after `UpdateTime`. The time must be greater than or equal
to `Time`. If a meter does not require an update then `UpdateTime` should be
`infinity`.

When updating the meter, `handle_update/5`:
```
-callback handle_update(QueueDelay :: non_neg_integer(),
                        ProcessDelay :: non_neg_integer(),
                        RelativeTime :: integer(), Time :: integer(),
                        State :: term()) ->
     {NState :: term(), UpdateTime :: integer() | infinity}.
```
`QueueDelay` is the approximate time a message spends in the message queue of
the process. `ProcessDelay` is the average time spent processing a message
since the last update. `RelativeTime` is an approximation of the
`RelativeTime` for an `ask` request if a match was to occur immediately. If
the process has not matched a request for a significant period of time this
value can grow large and become inaccurate.

The other variables are equivalent to those in `init/2`, with `NState` being
the new state.

When handling a message, `handle_info/3`:
```
-callback handle_info(Msg :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.
```
`Msg` is the message, and may be intended for another callback.

The other variables are equivalent to those in `init/2`, with `NState` being
the new state.

The other variables are equivalent to those in `init/3`, with `NState` being
the new state.

When changing the state due to a code change, `code_change/4`:
```
-callback code_change(OldVsn :: term(), Time :: integer(), State :: term(),
                      Extra :: term()) ->
     {NState :: term(), TimeoutTime :: integer() | infinity}.
```
On an upgrade `OldVsn` is version the state was created with and on an
downgrade is the same form except `{down, OldVsn}`. `OldVsn` is defined by
the vsn attribute(s) of the old version of the callback module. If no such
attribute is defined, the version is the checksum of the BEAM file. `Extra`
is from `{advanced, Extra}` in the update instructions.

The other variables are equivalent to those in `init/3`, with `NState` being
the new state.

When changing the configuration of a queue, `config_change/4`:
```
-callback config_change(Args :: term(), Time :: integer(), State :: term()) ->
     {NState :: term(), TimeoutTime :: integer() | infinity}.
```
The variables are equivalent to those in `init/2`, with `NState` being the
new state.

When cleaning up the meter, `terminate/2`:
```
-callback terminate(Reason :: sbroker_handlers:reason(), State :: term()) ->
     term().
```
`Reason` is `stop` if the meter is being shutdown, `change` if the meter is
being replaced by another meter, `{bad_return_value, Return}` if a previous
callback returned an invalid term or `{Class, Reason, Stack}` if a previous
callback raised an exception.

`State` is the current state of the meter.

The return value is ignored.
""").

%% private api

-export([code_change/6]).
-export([terminate/3]).

%% types

?DOC("Initialize meter with time and arguments.").
-callback init(Time :: integer(), Args :: term()) ->
    {State :: term(), UpdateTime :: integer() | infinity}.

?DOC("Handle meter update with delay and timing information.").
-callback handle_update(
    QueueDelay :: non_neg_integer(),
    ProcessDelay :: non_neg_integer(),
    RelativeTime :: integer(),
    Time :: integer(),
    State :: term()
) ->
    {NState :: term(), UpdateTime :: integer() | infinity}.

?DOC("Handle info message sent to meter.").
-callback handle_info(Msg :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), UpdateTime :: integer() | infinity}.

?DOC("Handle code change and migrate meter state.").
-callback code_change(
    OldVsn :: term(),
    Time :: integer(),
    State :: term(),
    Extra :: term()
) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle configuration change with new arguments.").
-callback config_change(Args :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), UpdateTime :: integer() | infinity}.

?DOC("Clean up meter resources on termination.").
-callback terminate(Reason :: sbroker_handlers:reason(), State :: term()) ->
    term().

%% private api

?DOC(false).
-spec code_change(Module, OldVsn, Send, Time, State, Extra) ->
    {NState, TimeoutTime}
when
    Module :: module(),
    OldVsn :: term(),
    Send :: integer(),
    Time :: integer(),
    State :: term(),
    Extra :: term(),
    NState :: term(),
    TimeoutTime :: integer() | infinity.
code_change(Mod, OldVsn, _, Time, State, Extra) ->
    Mod:code_change(OldVsn, Time, State, Extra).

?DOC(false).
-spec terminate(Module, Reason, State) -> term() when
    Module :: module(),
    Reason :: sbroker_handlers:reason(),
    State :: term().
terminate(Mod, Reason, State) ->
    Mod:terminate(Reason, State).
