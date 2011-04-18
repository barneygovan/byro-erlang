-module(fileshare_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_Type, StartArgs) ->
    fileshare_supervisor:start_link(StartArgs).

stop(_State) ->
    ok.
