-module(durden_transport_soap11_request_parser).
-export([get_request_args/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("erl_types.hrl").

-define('SOAP:Envelope', "{http://schemas.xmlsoap.org/soap/envelope/}Envelope").
-define('SOAP:Header', "{http://schemas.xmlsoap.org/soap/envelope/}Header").
-define('SOAP:Body', "{http://schemas.xmlsoap.org/soap/envelope/}Body").

-spec get_request_args( 
	ReqXml :: term(), 
	FuncName :: string(), 
	WSD :: #wsd{}
) -> [any()].

get_request_args( ReqXml, FuncName, WSD ) ->
	{ok, SoapBody} = extract_soap_body( ReqXml ),
	{ok, Message} = extract_soap_message( SoapBody ),
	{ok, _Args} = extract_func_args( Message, FuncName, WSD ).


extract_soap_body( {?'SOAP:Envelope', _, SoapEnvelopeContent } ) ->
	[ SoapBody ] = [ Body || Body = {?'SOAP:Body', _, _} <- SoapEnvelopeContent ],
	{ok, SoapBody}.

extract_soap_message( {?'SOAP:Body', _, [ Message ]} ) -> {ok, Message}.

extract_func_args(
	Message = {MsgType, _MsgAttrs, MsgChildren},
	FuncName, WSD = #wsd{ target_ns = TargetNS, schemas = Schemas }
) ->
	FuncsNS = durden_wsd_aux:resolve_ns( TargetNS, tns_funcs ),
	{ok, FuncsSchema} = ?dict_m:find(FuncsNS, Schemas),
	{ok, FuncDef} = ?dict_m:find(FuncName, FuncsSchema),
	FuncArgDefs = FuncDef #et_func.args,
	{ok, Matched} = match_func_args(MsgChildren, FuncArgDefs),
	{ok, _Deserialized} = deserialize_func_args(Matched, WSD).

match_func_args( MsgChildren, FuncArgDefs ) ->
	{Recognized, Unrecognized} = lists:foldl(
		fun({ArgName, ArgDef}, {Mapped, NotMapped}) ->
			ArgNameStr = atom_to_list(ArgName),
			[ ArgXml ] = [ AC || { AN, _, AC } <- NotMapped, AN == ArgNameStr ],
			Rest = [ A || A = { AN, _, _ } <- NotMapped, AN /= ArgNameStr ],
			{[ {ArgDef, ArgXml} | Mapped ], Rest }
		end,
		{ [], MsgChildren },
		FuncArgDefs),
	case Unrecognized of
		[] ->
			{ok, lists:reverse(Recognized)};
		[ _ | _ ] ->
			{error, unrecognized_args}
	end.


deserialize_func_args( ArgsSerialized, WSD ) ->
	Args = [ durden_xml_decode:decode( Def, Value, WSD ) || { Def, Value } <- ArgsSerialized ],
	{ok, Args}.

