%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_handler_wsdl).
-behaviour(durden_handler).

-export([try_handle/3]).

try_handle(Handler, BaseUrl, Req0) ->
	case cowboy_http_req:qs_val(<<"wsdl">>, Req0, undefined) of
		{ undefined, ReqQSRead } ->
			{next, ReqQSRead};
		{ _, ReqQSRead } ->
			{ok, XmlWSDL} = durden_wsd_cache:get_wsdl( Handler, BaseUrl ),
			{ok, ReqContentType} = cowboy_http_req:set_resp_header( <<"Content-Type">>, <<"text/xml; charset=utf-8">>, ReqQSRead),
			{ok, ReqReplied} = cowboy_http_req:reply(200, [], XmlWSDL, ReqContentType),
			{halt, ReqReplied}
	end.