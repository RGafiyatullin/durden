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
		[ BaseUrl ], Req
	) ->
	case cowboy_http_req:qs_val(<<"wsdl">>, Req, undefined) of
		{ <<"0">>, _ } ->
			{ok, XmlWSDL} = durden_wsd_cache:get_wsdl( Handler, BaseUrl ),
			{ok, ReqContentType} = cowboy_http_req:set_resp_header( <<"Content-Type">>, <<"text/xml; charset=utf-8">>, Req),
			{ok, _ReqReplied} = cowboy_http_req:reply(200, [], XmlWSDL, ReqContentType);
		{ undefined, _ } ->
			process_request( Handler, Req );
		_ ->
			error_400_bad_wsdl_arg( Req )
	end.

process_request( Handler, Req ) ->
	Transports = transport_modules(),
	TransportProbeResult = lists:foldl(fun
			(_, {will_handle, R, T}) ->
				{will_handle, R, T};

			(T, {wont_handle, R}) ->
				case T:can_handle( Handler, R ) of
					{ true, RWillHandle } -> { will_handle, RWillHandle, T };
					{ false, RWontHandle } -> { wont_handle, RWontHandle }
				end
		end,
		{wont_handle, Req},
		Transports
	),
	case TransportProbeResult of
		{will_handle, ReqWillHandle, ChosenTransport} ->
			{ok, Func, Args, ReqArgsParsed} = ChosenTransport:parse_request( Handler, Req ),
			case {ok, catch erlang:apply( Handler, Func, Args )} of
				{ok, RetValue} ->
					case catch ChosenTransport:render_response( Handler, RetValue, ReqArgsParsed ) of
						{ok, ReqResponded} -> {ok, ReqResponded};
						Error -> error_500_failed_to_render_positive_response( ReqArgsParsed, Error )
					end;
				Error ->
					case catch ChosenTransport:render_error( Error, ReqArgsParsed ) of
						{ok, ReqResponded} -> {ok, ReqResponded};
						_ErrorWhileRenderingError -> error_500_failed_to_fulfill_the_request( ReqArgsParsed, Error )
					end
			end;
		{wont_handle, ReqWontHandle} ->
			io:format("Error while transport detection~n"),
			error_400_bad_soap_transport( ReqWontHandle )
	end.

error_400_bad_wsdl_arg( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Bad 'wsdl' argument value">>, Req).

error_400_bad_soap_transport( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Unknown SOAP-transport">>, Req).

error_500_failed_to_render_positive_response( Req, Error ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(500, [], [ "Failed to render the response though : ", io_lib:format("~p", [Error]) ], Req).

error_500_failed_to_fulfill_the_request( Req, Error ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(500, [], [ "Failed to fulfill the request due to the following error: ", io_lib:format("~p", [Error]) ], Req).

transport_modules() -> [ durden_transport_soap11 ].


