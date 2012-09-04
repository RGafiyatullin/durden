-module(durden_wsd_cache).
-export([get_wsdl/2, get_wsd/1]).

-define(cache_srv, durden_wsd_cache_srv).

get_wsdl( Handler, BaseUrl ) ->
	{ok, XmlWSDL, _} = query_cache(Handler, BaseUrl),
	{ok, XmlWSDL}.

get_wsd( Handler ) ->
	{ok, _, Wsd} = query_cache(Handler, ""),
	{ok, Wsd}.


query_cache( Handler, BaseUrl ) ->
	gen_server:call(?cache_srv, {get_cache_entry, Handler, BaseUrl}, infinity).