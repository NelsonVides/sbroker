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

-spec start_link() -> {ok, Pid} when
    Pid :: pid().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor API

init([]) ->
    BetterServer =
        {sbetter_server, {sbetter_server, start_link, []}, permanent, 5000, worker, [
            sbetter_server
        ]},
    ProtectorServer =
        {sprotector_server, {sprotector_server, start_link, []}, permanent, 5000, worker, [
            sprotector_server
        ]},
    {ok, {{one_for_one, 3, 30}, [BetterServer, ProtectorServer]}}.
