%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_cowboy_http_soap).
-export([upgrade/4]).

-include("wsd.hrl").

-define(HTTP_Header_SoapAction, <<"Soapaction">>).

upgrade(
		_ListenerPid, Handler,
		_Opts, Req
	) ->
	case cowboy_http_req:qs_val(<<"wsdl">>, Req, undefined) of
		{ <<"0">>, _ } ->
			{ok, XmlWSDL} = durden_wsd_cache:get_wsdl( Handler ),
			{ok, ReqContentType} = cowboy_http_req:set_resp_header( <<"Content-Type">>, <<"text/xml; charset=utf-8">>, Req),
			{ok, _ReqReplied} = cowboy_http_req:reply(200, [], XmlWSDL, ReqContentType);
		{ undefined, _ } ->
			process_request( Handler, Req );
		_ ->
			error_400_bad_wsdl_arg( Req )
	end.
	
	% {ok, ReqReplied} = cowboy_http_req:reply(501, [], <<"Not implemented">>, Req).

process_request( Handler, Req ) ->
	Transports = transport_modules(),
	Result = lists:foldl(
		fun
			(_M,  {handled, Req0}) -> {handled, Req0};
			(M, {unhandled, Req0}) ->
				case M:try_handle(Handler, Req0) of
					{reject, Req1} -> {unhandled, Req1};
					{accept, Req1} -> {handled, Req1}
				end
		end,
		{unhandled, Req},
		Transports
	),
	case Result of
		{unhandled, Req2} -> error_400_bad_soap_transport(Req2);
		{handled, Req2} -> {ok, Req2}
	end.

error_400_bad_wsdl_arg( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Bad 'wsdl' argument value">>, Req).

error_400_bad_soap_transport( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Unknown SOAP-transport">>, Req).


transport_modules() -> [ durden_transport_soap11 ].


