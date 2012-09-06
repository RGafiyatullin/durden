%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

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

