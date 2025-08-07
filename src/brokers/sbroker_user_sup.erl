-module(sbroker_user_sup).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC(false).

-behaviour(supervisor).

%% public API

-export([start/2]).
-export([restart/2]).
-export([terminate/2]).
-export([delete/2]).
-export([which_children/1]).
-export([start_link/0]).

%% supervisor API

-export([init/1]).

%% public API

-spec start(Module, Name) -> {ok, Pid} | {error, Reason} when
    Module :: sbroker_user | sregulator_user,
    Name :: sbroker:name() | sregulator:name(),
    Pid :: pid() | undefined,
    Reason :: term().
start(Module, Name) ->
    supervisor:start_child(?MODULE, child(Module, Name)).

-spec restart(Module, Name) -> {ok, Pid} | {error, Reason} when
    Module :: sbroker_user | sregulator_user,
    Name :: sbroker:name() | sregulator:name(),
    Pid :: pid() | undefined,
    Reason :: term().
restart(Module, Name) ->
    supervisor:restart_child(?MODULE, {Module, Name}).

-spec terminate(Module, Name) -> ok | {error, not_found} when
    Module :: sbroker_user | sregulator_user,
    Name :: sbroker:name() | sregulator:name().
terminate(Module, Name) ->
    supervisor:terminate_child(?MODULE, {Module, Name}).

-spec delete(Module, Name) -> ok | {error, Reason} when
    Module :: sbroker_user | sregulator_user,
    Name :: sbroker:name() | sregulator:name(),
    Reason :: running | restarting | not_found.
delete(Module, Name) ->
    supervisor:delete_child(?MODULE, {Module, Name}).

-spec which_children(Module) -> [{Name, Pid, Type, Modules}] when
    Module :: sbroker_user | sregulator_user,
    Name :: sbroker:name() | sregulator:name(),
    Pid :: undefined | pid(),
    Type :: worker,
    Modules :: dynamic.
which_children(Module) ->
    [
        {Name, Pid, Type, Modules}
     || {{Mod, Name}, Pid, Type, Modules} <- supervisor:which_children(?MODULE),
        Mod == Module
    ].

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

%% supervisor API

-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 30},
    {ok, {SupFlags, children()}}.

%% internal

children() ->
    Env = application:get_all_env(sbroker),
    Brokers = proplists:get_value(brokers, Env, []),
    Regulators = proplists:get_value(regulators, Env, []),
    brokers(Brokers) ++ regulators(Regulators).

brokers(Brokers) ->
    children(sbroker_user, Brokers).

regulators(Regulators) ->
    children(sregulator_user, Regulators).

children(Module, List) ->
    [child(Module, Name) || {Name, _} <- List].

child(Module, Name) ->
    #{
        id => {Module, Name},
        start => {Module, start_link, [Name]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => dynamic
    }.
