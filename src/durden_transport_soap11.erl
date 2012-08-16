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
	WSD = #wsd{
		target_ns = TargetNS,
		schemas = Schemas
	}, 
	Handler, Req0
) ->
	FuncsNS = durden_wsd_aux:resolve_ns( TargetNS, tns_funcs ),
	{ok, FuncsSchema} = ?dict_m:find(FuncsNS, Schemas),
	{ok, FuncDef} = ?dict_m:find(FuncName, FuncsSchema),
	io:format("function ~p :: ~p~n", [FuncName, FuncDef]),
	{ ok, ReqBody, Req1 } = cowboy_http_req:body(Req0),
	{ ok, ReqXml, [] } = erlsom:simple_form( iolist_to_binary(ReqBody) ),
	io:format("Req SOAP-Envelope: ~p~n", [ ReqXml ]),
	{ok, ReqArgs} = ?req_parser:get_request_args( ReqXml, {FuncName, FuncsNS}, WSD ),
	io:format("~p:~p ~p~n", [Handler, list_to_existing_atom(FuncName), ReqArgs]),
	{reject, Req1}.

	

error_bad_soap_action(SA, TNS, Req0) ->
	{accept, _ReqReplied} = cowboy_http_req:reply(
		400, [], 
		["Could not match soap action '", SA, "' with the target namespace '", TNS, "'"],
		Req0
	).
