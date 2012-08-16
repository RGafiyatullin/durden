-module(durden_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	durden_sup:start_link().

stop(_State) -> ok.
