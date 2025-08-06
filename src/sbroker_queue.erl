-module(sbroker_queue).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Behaviour for implementing queues for `sbroker` and `sregulator`.

A custom queue must implement the `sbroker_queue` behaviour. The first
callback is `init/3`, which starts the queue:
```
-callback init(InternalQueue :: internal_queue(), Time :: integer(),
               Args :: term()) ->
     {State :: term(), TimeoutTime :: integer() | infinity}.
```
`InternalQueue` is the internal queue of requests, it is a `queue:queue()`
with items of the form `{SendTime, From, Value, Reference}`. `SendTime` is
the approximate time the request was sent in `native` time units and is
always less than or equal to `Time`.`From` is the a 2-tuple containing the
senders pid and a response tag. `SendTime` and `From` can be used with
`drop/3` to drop a request. `Value` is any term, `Reference` is the monitor
reference of the sender.

`Time` is the time, in `native` time units, of the queue at creation. Some
other callbacks will receive the current time of the queue as the second last
argument. It is monotically increasing, so subsequent calls will have the
same or a greater time.

`Args` is the arguments for the queue. It can be any term.

`State` is the state of the queue and used in the next call.

`TimeoutTime` represents the next time a queue wishes to call
`handle_timeout/2` to drop items. If a message is not received the timeout
should occur at or after `TimeoutTime`. The time must be greater than or
equal to `Time`. If a queue does not require a timeout then `TimeoutTime`
should be `infinity`. The value may be ignored or unavailable in other
callbacks if the queue is empty.

When inserting a request into the queue, `handle_in/6`:
```
-callback handle_in(SendTime :: integer(),
                    From :: {Sender :: pid(), Tag :: term()}, Value :: term(),
                    Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.
```
The variables are equivalent to those in `init/3`, with `NState` being the
new state.

When removing a request from the queue, `handle_out/2`:
```
-callback handle_out(Time :: integer(), State :: term()) ->
    {SendTime :: integer(), From :: {Sender :: pid(), Tag :: term()},
     Value:: term(), Ref :: reference, NState :: term(),
     TimeoutTime :: integer() | infinity} |
    {empty, NState :: term()}.
```

The variables are equivalent to those in `init/3`, with `NState` being the
new state. This callback either returns a single request, added in the
`InternalQueue` from `init/3` or enqueued with `handle_in/6`. If the queue is
empty an `empty` tuple is returned.

When a timeout occurs, `handle_timeout/2`:
```
-callback handle_timeout(Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.
```
The variables are equivalent to those in `init/3`, with `NState` being the
new state.

When cancelling requests, `handle_cancel/3`:
```
-callback handle_cancel(Tag :: term(), Time :: integer(), State :: term()) ->
    {Reply :: false | pos_integer(), NState :: term(),
     TimeoutTime :: integer() | infinity}.
```
`Tag` is a response tag, which is part of the `From` tuple passed via
`InternalQueue` in `init/3` or directly in `handle_in/6`. There may be
multiple requests with the same tag and all should be removed.

If no requests are cancelled the `Reply` is `false`, otherwise it is the
number of cancelled requests.

The other variables are equivalent to those in `init/3`, with `NState` being
the new state.

When handling a message, `handle_info/3`:
```
-callback handle_info(Msg :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.
```
`Msg` is the message, and may be intended for another queue.

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
The variables are equivalent to those in `init/3`, with `NState` being the
new state.

When returning the number of queued requests, `len/1`:
```
-callback len(State :: term()) -> Len :: non_neg_integer().
```
`State` is the current state of the queue and `Len` is the number of queued
requests. This callback must be idempotent and so not drop any requests.

When returning the send time of the oldest request in the queue,
`send_time/1`:
```
-callback send_time(State :: term()) -> SendTime :: integer() | empty.
```
`State` is the current state of the queue and `SendTime` is the send time of
the oldest request, if not requests then `empty`. This callback must be
idempotent and so not drop any requests.

When cleaning up the queue, `terminate/2`:
```
-callback terminate(Reason :: sbroker_handlers:reason(), State :: term()) ->
     InternalQueue :: internal_queue().
```
`Reason` is `stop` if the queue is being shutdown, `change` if the queue is
being replaced by another queue, `{bad_return_value, Return}` if a previous
callback returned an invalid term or `{Class, Reason, Stack}` if a previous
callback raised an exception.

`State` is the current state of the queue.

`InternalQueue` is the same as `init/3` and is passed to the next queue if
`Reason` is `change`.

The process controlling the queue may not be terminating with the queue and
so `terminate/2` should do any clean up required.
""").

-behaviour(sbroker_handlers).

%% public api

-export([drop/3]).

%% sbroker_handlers api

-export([initial_state/0]).
-export([init/5]).
-export([code_change/6]).
-export([config_change/5]).
-export([terminate/3]).

%% types

?DOC("Internal queue structure containing timestamped entries with process references.").
-type internal_queue() ::
    queue:queue({integer(), {pid(), term()}, term(), reference()}).

-export_type([internal_queue/0]).

?DOC("Initialize queue with given arguments and return initial state.").
-callback init(Q :: internal_queue(), Time :: integer(), Args :: term()) ->
    {State :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle incoming request and update queue state.").
-callback handle_in(
    SendTime :: integer(),
    From :: {Sender :: pid(), Tag :: term()},
    Value :: term(),
    Time :: integer(),
    State :: term()
) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle outgoing request from queue and return next item or empty.").
-callback handle_out(Time :: integer(), State :: term()) ->
    {
        SendTime :: integer(),
        From :: {pid(), Tag :: term()},
        Value :: term(),
        Ref :: reference(),
        NState :: term(),
        TimeoutTime :: integer() | infinity
    }
    | {empty, NState :: term()}.

?DOC("Handle timeout event and update queue state.").
-callback handle_timeout(Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle request cancellation and return success status.").
-callback handle_cancel(Tag :: term(), Time :: integer(), State :: term()) ->
    {Reply :: false | pos_integer(), NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle info message and update queue state.").
-callback handle_info(Msg :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle code change and migrate state to new version.").
-callback code_change(
    OldVsn :: term(),
    Time :: integer(),
    State :: term(),
    Extra :: term()
) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Handle configuration change and update queue state.").
-callback config_change(Args :: term(), Time :: integer(), State :: term()) ->
    {NState :: term(), TimeoutTime :: integer() | infinity}.

?DOC("Return current length of the queue.").
-callback len(State :: term()) -> Len :: non_neg_integer().

?DOC("Return send time of next item or empty if queue is empty.").
-callback send_time(State :: term()) -> SendTime :: integer() | empty.

?DOC("Clean up queue resources and return final queue state.").
-callback terminate(Reason :: sbroker_handlers:reason(), State :: term()) ->
    Q :: internal_queue().

%% public api

?DOC("""
Drop a request from `From`, sent at `SendTime` from the queue.

Call `drop/3` when dropping a request from a queue.
""").
-spec drop(From, SendTime, Time) -> ok when
    From :: {pid(), Tag :: term()},
    SendTime :: integer(),
    Time :: integer().
drop(From, SendTime, Time) ->
    _ = gen:reply(From, {drop, Time - SendTime}),
    ok.

%% sbroker_handlers api

?DOC(false).
-spec initial_state() -> Q when
    Q :: internal_queue().
initial_state() ->
    queue:new().

?DOC(false).
-spec init(Module, Q, Send, Time, Args) -> {State, TimeoutTime} when
    Module :: module(),
    Q :: internal_queue(),
    Send :: integer(),
    Time :: integer(),
    Args :: term(),
    State :: term(),
    TimeoutTime :: integer() | infinity.
init(Mod, Q, _, Now, Args) ->
    Mod:init(Q, Now, Args).

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
-spec config_change(Module, Args, Send, Time, State) ->
    {NState, TimeoutTime}
when
    Module :: module(),
    Args :: term(),
    Send :: integer(),
    Time :: integer(),
    State :: term(),
    NState :: term(),
    TimeoutTime :: integer() | infinity.
config_change(Mod, Args, _, Now, State) ->
    Mod:config_change(Args, Now, State).

?DOC(false).
-spec terminate(Module, Reason, State) -> Q when
    Module :: module(),
    Reason :: sbroker_handlers:reason(),
    State :: term(),
    Q :: internal_queue().
terminate(Mod, Reason, State) ->
    Q = Mod:terminate(Reason, State),
    case queue:is_queue(Q) of
        true -> Q;
        false -> exit({bad_return_value, Q})
    end.
