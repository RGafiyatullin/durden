%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_cowboy_http_soap).
-export([upgrade/4]).

upgrade(
		_ListenerPid, Handler,
		_Opts, Req
	) ->
	case cowboy_http_req:qs_val(<<"wsdl">>, Req, undefined) of
		{ <<"0">>, _ } ->
			generate_wsdl( Handler, Req );
		{ undefined, _ } ->
			process_request( Handler, Req );
		_ ->
			error_400_bad_wsdl_arg( Req )
	end.
	
	% {ok, ReqReplied} = cowboy_http_req:reply(501, [], <<"Not implemented">>, Req).

generate_wsdl( Handler, Req ) ->
	TxtWSDL = durden_wsdl:module_wsdl("http://localhost:8080/test/service.asmx", Handler),
	{ok, ReqContentType} = cowboy_http_req:set_resp_header( <<"Content-Type">>, <<"text/xml; charset=utf-8">>, Req),
	{ok, _ReqReplied} = cowboy_http_req:reply(200, [], TxtWSDL, ReqContentType).

process_request( _Handler, Req0 ) ->
	io:format("Req: ~p~n", [Req0]),
	{BodyLen, Req1} = cowboy_http_req:body_length(Req0),
	io:format("Body-Len: ~p~n", [BodyLen]),
	{ok, Body, Req2} = cowboy_http_req:body(Req1),
	io:format("Body: ~p~n", [Body]),

	{ok, _ReqReplied} = cowboy_http_req:reply(501, [], <<"Not implemented">>, Req2).

error_400_bad_wsdl_arg( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Bad 'wsdl' argument value">>, Req).

