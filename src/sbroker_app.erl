-module(sbroker_app).
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.
?MODULEDOC(false).

-behaviour(application).

%% application API

-export([start/2]).
-export([stop/1]).

%% application API

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_, _) ->
    sbroker_sup:start_link().

-spec stop(term()) -> ok.
stop(_) ->
    ok.
