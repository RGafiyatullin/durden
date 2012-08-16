-module(durden_wsd_cache).
-export([get_wsdl/1, get_wsd/1]).

-define(cache_srv, durden_wsd_cache_srv).

get_wsdl( Handler ) ->
	{ok, XmlWSDL, _} = query_cache(Handler),
	{ok, XmlWSDL}.

get_wsd( Handler ) ->
	{ok, _, Wsd} = query_cache(Handler),
	{ok, Wsd}.


query_cache( Handler ) ->
	gen_server:call(?cache_srv, {get_cache_entry, Handler}, infinity).