-module(durden_transport_soap11).
-behaviour(durden_transport).
% -export([try_handle/2]).
-export([
	can_handle/2,
	parse_request/2,
	render_response/3,
	render_error/2
	]).

-include("wsd.hrl").

-define(req_parser, durden_transport_soap11_request_parser).
-define(resp_composer, durden_transport_soap11_response_composer).

-spec can_handle( 
	Handler :: atom(), 
	Req :: term()
) -> 
	{boolean(), ReqUsed :: term()}.
-spec parse_request( 
	Handler :: atom(),
	Req :: term() 
) -> 
	{ ok, F :: atom(), A :: [ term() ], ReqUsed :: term() }.
-spec render_response(
	Handler :: atom(),
	RetValue :: term(),
	Req :: term()
) -> 
	{ok, ReqResponded :: term()}.
-spec render_error(
	Error :: term(),
	Req :: term()
	) ->
	{ok, ReqResponded :: term()}.

can_handle( H, Req0 ) ->
	case find_soap_action( Req0 ) of
		{undefined, Req1} -> {false, Req1};
		{SoapAction, Req1} -> {true, Req1}
	end.

render_error( Error, Req0 ) ->
	{ok, _ReqReplied} = cowboy_http_req:reply(501, [], <<"SOAP-compatible error rendering not implemented">>, Req0).


parse_request( H, Req0 ) ->
	{ok, WSD} = durden_wsd_cache:get_wsd(H),
	{FuncName, Req1} = get_func_name(H, Req0),
	{ok, Args, Req2} = parse_call_arguments( FuncName, WSD, Req1 ),
	{ok, list_to_existing_atom(FuncName), Args, Req2}.

render_response( H, RetValue, Req0 ) ->
	{ok, WSD} = durden_wsd_cache:get_wsd(H),
	{FuncName, Req1} = get_func_name( H, Req0 ),
	{ok, XmlResponseEnvelope} = ?resp_composer:get_response_envelope( RetValue, FuncName, WSD ),
	{ok, Req2} = cowboy_http_req:set_resp_header( <<"Content-Type">>, <<"text/xml; charset=utf-8">>, Req1),
	{ok, Req3} = cowboy_http_req:reply( 200, [], XmlResponseEnvelope, Req2 ),
	Req3.


get_func_name( H, Req0 ) ->
	{ok, WSD} = durden_wsd_cache:get_wsd(H),
	TargetNS = list_to_binary( WSD #wsd.target_ns ),
	{SA, Req1} = find_soap_action( Req0 ),
	SizeOfTNS = size(TargetNS),
	case binary:longest_common_prefix([ SA, TargetNS ]) of
		SizeOfTNS ->
			FuncName = binary_to_list(
				binary:replace( 
					binary:replace(SA, TargetNS, <<>>), 
					<<"/">>, <<>>
				)
			),
			{FuncName, Req1};
		_ -> 
			throw({error, bad_soap_action, SA})
	end.


parse_call_arguments( FuncName, WSD, Req0 ) ->
	{ ok, ReqBody, Req1 } = cowboy_http_req:body(Req0),
	{ ok, ReqXml, [] } = erlsom:simple_form( iolist_to_binary(ReqBody) ),
	{ ok, ReqArgs } = ?req_parser:get_request_args( ReqXml, FuncName, WSD ),
	{ok, ReqArgs, Req1}.


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
