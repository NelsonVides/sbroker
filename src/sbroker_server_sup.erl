-module(sbroker_server_sup).
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

-export([start_link/0]).

%% supervisor API

-export([init/1]).

%% public API

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

%% supervisor API

-spec init(noargs) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(noargs) ->
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 30},
    BetterServer = #{
        id => sbetter_server,
        start => {sbetter_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [sbetter_server]
    },
    ProtectorServer = #{
        id => sprotector_server,
        start => {sprotector_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [sprotector_server]
    },
    {ok, {SupFlags, [BetterServer, ProtectorServer]}}.
