-module(sbroker_sup).
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

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ServerSup =
        {sbroker_server_sup, {sbroker_server_sup, start_link, []}, permanent, infinity, supervisor,
            [sbroker_server_sup]},
    UserSup =
        {sbroker_user_sup, {sbroker_user_sup, start_link, []}, permanent, infinity, supervisor, [
            sbroker_user_sup
        ]},
    {ok, {{rest_for_one, 3, 300}, [ServerSup, UserSup]}}.
