-module(durden_transport_soap11_response_composer).
-export([get_response_envelope/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

-define(xml_enc, durden_xml_encode).

-spec get_response_envelope( 
	RetValue :: any(), 
	FuncName :: string(), 
	WSD :: #wsd{}
) -> {ok, iolist()}.
get_response_envelope(
	RetValue, 
	FuncName, 
	WSD = #wsd{ 
		target_ns = TargetNS,
		schemas = Schemas
	}
) ->
	io:format("FuncName: ~p~n", [FuncName]),
	Envelope = ?xml:node({"Envelope", ?XML_NS_SOAPENV}, [], [
		?xml:node({"Body", ?XML_NS_SOAPENV}, [], [
			create_response_message(RetValue, FuncName, WSD)
		])
	]),

	PredefinedSchemaPrefixes = ?dict_m:from_list([
		{?XML_NS_WSDL, "wsdl"},
		{?XML_NS_SOAP, "soap"},
		{?XML_NS_SOAP12, "soap12"},
		{?XML_NS_SOAPENV, "soapenv"},
		{?XML_NS_SOAPENC, "soapenc"},
		{?XML_NS_HTTP, "http"},
		{?XML_NS_MIME, "mime"},
		{?XML_NS_MS_TM, "mstm"},
		{?XML_NS_XSD, "xs"},
		{?XML_NS_MS, "ms"},
		{TargetNS, "tns"}
	]),
	{ SchemaPrefixes, _ } = ?dict_m:fold(
		fun( NS, _SchemaTypes, { PSP, Idx } ) ->
			case ?dict_m:find( NS, PSP ) of
				{ok, _} -> { PSP, Idx };
				error ->
					P = "s" ++ integer_to_list(Idx),
					{ ?dict_m:store( NS, P, PSP ), Idx + 1 }
			end
		end,
		{ PredefinedSchemaPrefixes, 0 },
		Schemas
		),
	Envelope_NSsImported = ?dict_m:fold(
		fun( NS, Prefix, Env ) ->
			?xml:imp_ns(NS, Prefix, Env)
		end,
		Envelope,
		SchemaPrefixes
	),
	XmlEnvelope = ?xml:render( Envelope_NSsImported ),
	{ok, XmlEnvelope}.


-spec create_response_message(RetValue :: any(), FuncName :: string(), WSD :: #wsd{}) -> ?xml:xml_node().
create_response_message(
	RetValue, 
	FuncName, 
	WSD = #wsd{
		target_ns = TargetNS,
		schemas = Schemas
	}
) ->
	RespMessageName = FuncName ++ ?WSDL_SUFFIX_RESPONSE,
	RespMessageNS = durden_wsd_aux:resolve_ns( TargetNS, tns_funcs ),
	{ok, RespMessageSchema} = ?dict_m:find(RespMessageNS, Schemas),
	{ok, FuncDef} = ?dict_m:find(
						FuncName, 
						RespMessageSchema
					),
	RespDef = FuncDef #et_func.ret,
	io:format("Response Def: ~p~n", [RespDef]),
	RespMessageNodeContent = ?xml_enc:encode(RetValue, RespDef, WSD),
	?xml:node({RespMessageName, RespMessageNS}, [], [
		?xml:node({?WSDL_FIELD_RESULT_AS_STR(FuncName), RespMessageNS},
			[],
			[ RespMessageNodeContent ]
		)
	]).
