%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

%%%
%%% Durden's root supervisor.
%%% Durden does not have a large supervision tree as it does not need to: Cowboy handles all this kind of work.
%%% The only shared across the handlers thing is the cache of WSDs (Web-service definitions).
%%%

-module(durden_sup).
-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
	{ok,
		{
			{one_for_one, 5, 30},
			[
				{wsd_cache, {durden_wsd_cache_srv, start_link, []}, permanent, 5000, worker, [durden_wsd_cache_srv]}
			]
		}
	}.

