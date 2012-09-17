%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

%%%
%%% Accessor to the cache of WSDs.
%%% When we need raw-WSD only we lookup for it by the handler's module name.
%%% Rendered WSDL does require the URI of the endpoint ready to handle actual SOAP-requests. 
%%%  Hence we lookup for WSDLs by a composite key - {HandlerModule, BaseUrl}.
%%%

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