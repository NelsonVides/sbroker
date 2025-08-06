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

start(_, _) ->
    sbroker_sup:start_link().

stop(_) ->
    ok.
