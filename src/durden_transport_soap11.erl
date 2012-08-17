-module(durden_transport_soap11).
-behaviour(durden_transport).
-export([try_handle/2]).

-include("wsd.hrl").

-define(req_parser, durden_transport_soap11_request_parser).

-spec try_handle(Handler :: atom(), Req :: term() ) -> 
	{accept, Req :: term()} |
	{reject, Req :: term()}.


try_handle(H, Req0) ->
	case find_soap_action(Req0) of
		{undefined, Req1} -> {reject, Req1};
		{SoapAction, Req1} -> handle_soap_action(SoapAction, H, Req1)
	end.

find_soap_action(Req0) ->
	{AllHeaders, Req1} = cowboy_http_req:headers(Req0),
	case lists:filter(fun
		({BName, Value}) when is_binary(BName) ->
			SName = binary_to_list(BName),
			case string:to_lower(SName) of
				"soapaction" -> true;
				_ -> false
			end;
		(_) -> false
	end, AllHeaders) of
		[] -> {undefined, Req1};
		[ {_, Value} ] -> 
			TrimmedValue = de_quote(Value),
			{TrimmedValue, Req1}
	end.

de_quote( Unclean ) -> binary:replace(Unclean, <<"\"">>, <<>>, [global]).

handle_soap_action( SA, Handler, Req0 ) ->
	{ok, WSD} = durden_wsd_cache:get_wsd(Handler),
	TargetNS = list_to_binary( WSD #wsd.target_ns ),
	SizeOfTNS = size(TargetNS),
	case binary:longest_common_prefix([ SA, TargetNS ]) of
		SizeOfTNS ->
			FuncName = binary_to_list(
							binary:replace( 
								binary:replace(SA, TargetNS, <<>>), 
								<<"/">>, <<>> )),
			handle_func_call( FuncName, WSD, Handler, Req0 );
		_ -> error_bad_soap_action(SA, TargetNS, Req0)
	end.

handle_func_call( 
	FuncName, 
	WSD = #wsd{}, 
	Handler, Req0
) ->
	{ ok, ReqBody, Req1 } = cowboy_http_req:body(Req0),
	{ ok, ReqXml, [] } = erlsom:simple_form( iolist_to_binary(ReqBody) ),
	{ ok, ReqArgs } = ?req_parser:get_request_args( ReqXml, FuncName, WSD ),
	case catch {ok, erlang:apply(Handler, list_to_existing_atom(FuncName), ReqArgs)} of
		{ok, RetValue} ->
			io:format("RetValue: ~p~n", [ RetValue ]),
			{ reject, Req1 };
		Error ->
			io:format("Error invoking the target module: ~p~n", [ Error ]),
			{ reject, Req1 }
	end.

	

error_bad_soap_action(SA, TNS, Req0) ->
	{accept, _ReqReplied} = cowboy_http_req:reply(
		400, [], 
		["Could not match soap action '", SA, "' with the target namespace '", TNS, "'"],
		Req0
	).
