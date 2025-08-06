-module(sbetter_server).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC("""
Server for storing the `ask` and `ask_r` values for load balanacing
processes with `sbetter`.
""").

-behaviour(gen_server).

%% public API

-export([register/3]).
-export([unregister/1]).
-export([update/3]).

%% private API

-export([start_link/0]).
-export([lookup/2]).

%% gen_server API

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%% macros

-define(TIMEOUT, 5000).

%% public API

?DOC("""
Register the local process `Pid` with the server and sets the `ask` and
`ask_r` values (such as sojourn times) as integer values `AskValue` and
`AskRValue`.

Returns `true` if the process is successfully registered, or `false` if
already registered.
""").
-spec register(Pid, AskValue, AskRValue) -> Result when
    Pid :: pid(),
    AskValue :: integer(),
    AskRValue :: integer(),
    Result :: boolean().
register(Pid, AskValue, BidValue) when
    is_pid(Pid),
    node(Pid) == node(),
    is_integer(AskValue),
    is_integer(BidValue)
->
    gen_server:call(?MODULE, {register, Pid, AskValue, BidValue}, ?TIMEOUT).

?DOC("""
Unregister the process `Pid` with the server.

The server will synchronously unlink from `Pid`.
""").
-spec unregister(Pid) -> true when
    Pid :: pid().
unregister(Pid) when is_pid(Pid), node(Pid) == node() ->
    gen_server:call(?MODULE, {unregister, Pid}, ?TIMEOUT).

?DOC("""
Update the `ask` and `ask_r`  with values (such as sojourn times) as
integer values `AskValue` and `AskRValue` for `Pid`.

Returns `true` if the values were updated, or `false` if `Pid` is not
registered with the server.
""").
-spec update(Pid, AskValue, AskRValue) -> Result when
    Pid :: pid(),
    AskValue :: integer(),
    AskRValue :: integer(),
    Result :: boolean().
update(Pid, AskValue, BidValue) when
    is_integer(AskValue), is_integer(BidValue)
->
    ets:update_element(?MODULE, {Pid, ask}, {2, AskValue}) andalso
        ets:update_element(?MODULE, {Pid, bid}, {2, BidValue}).

%% private API

?DOC(false).
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

?DOC(false).
-spec lookup(Pid, Key) -> SojournTime when
    Pid :: pid(),
    Key :: ask | ask_r,
    SojournTime :: non_neg_integer().
lookup(Pid, ask) ->
    ets:update_counter(?MODULE, {Pid, ask}, {2, 0});
lookup(Pid, ask_r) ->
    ets:update_counter(?MODULE, {Pid, bid}, {2, 0}).

%% gen_server API

?DOC(false).
-spec init(atom()) -> {ok, ets:table()}.
init(Table) ->
    _ = process_flag(trap_exit, true),
    Table = ets:new(
        Table,
        [named_table, set, public, {write_concurrency, auto}]
    ),
    {ok, Table}.

?DOC(false).
-spec handle_call(dynamic(), gen_server:from(), ets:table()) ->
    {reply, term(), ets:table()} | {stop, term(), ets:table()}.
handle_call({register, Pid, AskValue, BidValue}, _, Table) ->
    link(Pid),
    {reply, insert_new(Table, Pid, AskValue, BidValue), Table};
handle_call({unregister, Pid}, _, Table) ->
    delete(Table, Pid),
    unlink(Pid),
    {reply, true, Table};
handle_call(Call, _, Table) ->
    {stop, {bad_call, Call}, Table}.

?DOC(false).
-spec handle_cast(term(), ets:table()) -> {stop, term(), ets:table()}.
handle_cast(Cast, Table) ->
    {stop, {bad_cast, Cast}, Table}.

?DOC(false).
-spec handle_info(term(), ets:table()) -> {noreply, ets:table()}.
handle_info({'EXIT', Pid, _}, Table) ->
    delete(Table, Pid),
    {noreply, Table};
handle_info(Msg, Table) ->
    error_logger:error_msg(
        "sbetter_server received unexpected message: ~p~n",
        [Msg]
    ),
    {noreply, Table}.

%% Helpers

insert_new(Table, Pid, AskValue, BidValue) ->
    ets:insert_new(Table, {{Pid, ask}, AskValue}) andalso
        ets:insert_new(Table, {{Pid, bid}, BidValue}).

delete(Table, Pid) ->
    ets:delete(Table, {Pid, ask}),
    ets:delete(Table, {Pid, bid}).
