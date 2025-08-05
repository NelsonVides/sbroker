%%-------------------------------------------------------------------
%%
%% Copyright (c) 2016, James Fish <james@fishcakez.com>
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%-------------------------------------------------------------------
-module(sregulator_relative_valve_statem).

-include_lib("proper/include/proper.hrl").

-export([module/0]).
-export([args/0]).
-export([init/3]).
-export([handle_update/3]).
-export([handle_ask/2]).
-export([handle_done/2]).
-export([handle/2]).
-export([config_change/4]).

module() ->
    sregulator_relative_valve.

args() ->
    ?LET(
        {Target, Min, Max},
        gen_args(),
        #{target => Target, min => Min, max => Max}
    ).

gen_args() ->
    ?LET(
        {Min, Max},
        ?SUCHTHAT(
            {Min, Max},
            {choose(0, 5), oneof([choose(0, 5), infinity])},
            Min =< Max
        ),
        {choose(-10, 10), Min, Max}
    ).

init(#{target := Target, min := Min, max := Max}, _, _) ->
    NTarget = erlang:convert_time_unit(Target, milli_seconds, native),
    {Min, Max, closed, {NTarget, undefined}}.

handle_update(Value, Time, {Target, _}) ->
    handle(Time, {Target, Value}).

handle_ask(Time, {Target, _}) ->
    handle(Time, {Target, undefined}).

handle_done(Time, State) ->
    handle(Time, State).

handle(_, {Target, Value} = State) when Target > Value ->
    {open, State};
handle(_, State) ->
    {closed, State}.

config_change(
    #{target := Target, min := Min, max := Max},
    _,
    Time,
    {_, Value}
) ->
    NTarget = erlang:convert_time_unit(Target, milli_seconds, native),
    {Status, State} = handle(Time, {NTarget, Value}),
    {Min, Max, Status, State}.
