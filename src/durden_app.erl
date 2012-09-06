%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	durden_sup:start_link().

stop(_State) -> ok.
