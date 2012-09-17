%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

%%%
%%% When request is being upgraded we first pass the request to 'handlers' (durden_handler behaviour).
%%% If none of the latter accepts the request - we are piping the request through the transports available.
%%% Transports are offered to handle a request. If any accepts the offer it is to parse the request and report what function and with which args to be called.
%%% After the request is fulfiled or not (e.g. an error occured) - we ask the chosen transport to render a successful or negative result.
%%%

-module(durden_cowboy_handler).
-export([upgrade/4]).

-include("wsd.hrl").

-define(HTTP_Header_SoapAction, <<"Soapaction">>).

handler_modules() -> [ durden_handler_wsdl, durden_handler_doc ].
transport_modules() -> [ durden_transport_soap12 ].

upgrade(
		_ListenerPid, Module,
		[ BaseUrl ], Req0
	) ->
	% try
		Handlers = handler_modules(),
		case lists:foldl(
			fun
				(_H, {halt, Req}) -> {halt, Req};
				(H, {next, Req}) ->
					H:try_handle(Module, BaseUrl, Req)
			end,
			{next, Req0}, Handlers
		) of
			{halt, ReqReplied} -> {ok, ReqReplied};
			{next, Req1} ->
				process_soap_request( Module, Req1 )
		end.
	% catch
	% 	ErrType:Err ->
	% 		?log_crit([
	% 			"Unhandled error",
	% 			{err_type, ErrType},
	% 			{error, Err}
	% 			]),
	% 		error_500_severe( Req0, ErrType, Err )
	% end.

process_soap_request( Handler, Req ) ->
	Transports = transport_modules(),
	TransportProbeResult = lists:foldl(fun
			(_, {halt, R, T}) ->
				{halt, R, T};

			(T, {next, R}) ->
				case T:can_handle( Handler, R ) of
					{ true, RWillHandle } -> { halt, RWillHandle, T };
					{ false, RWontHandle } -> { next, RWontHandle }
				end
		end,
		{next, Req},
		Transports
	),
	case TransportProbeResult of
		{halt, ReqWillHandle, ChosenTransport} ->
			{ok, Func, Args, ReqArgsParsed} = ChosenTransport:parse_request( Handler, Req ),
			case catch {ok, erlang:apply( Handler, Func, Args )} of
				{ok, RetValue} ->
					case catch ChosenTransport:render_response( Handler, RetValue, ReqArgsParsed ) of
						{ok, ReqResponded} -> {ok, ReqResponded};
						Error -> error_500_failed_to_render_positive_response( ReqArgsParsed, RetValue, Error )
					end;
				Error ->
					case catch ChosenTransport:render_error( Error, ReqArgsParsed ) of
						{ok, ReqResponded} -> {ok, ReqResponded};
						FailedToRenderError -> 
							?log_error([
								"Failed to render error",
								{render_error, FailedToRenderError},
								{app_error, Error}
							]),
							error_500_failed_to_fulfill_the_request( ReqArgsParsed, Error )
					end
			end;
		{next, ReqWontHandle} ->
			?log_warn([
				"Error while transport detection", 
				{transports_available, Transports}
			]),
			error_400_bad_soap_transport( ReqWontHandle )
	end.

error_400_bad_soap_transport( Req ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(400, [], <<"Unknown SOAP-transport">>, Req).

error_500_failed_to_render_positive_response( Req, RetValue, Error ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(500, [], 
		[
			"Failed to render the response though request has been understood: ", 
			io_lib:format("~p", [Error]),
			io_lib:format("~nRetValue: ~p", [RetValue])
		], Req).

error_500_failed_to_fulfill_the_request( Req, Error ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(500, [], 
		[ 
			"Failed to fulfill the request due to the following error: ", 
			io_lib:format("~p", [Error]) 
		], Req).

error_500_severe( Req, ErrType, Err ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(500, [], 
		[ 
			"Something went very wrong: ",
			io_lib:format("~p:~p", [ErrType, Err])
		], Req).



