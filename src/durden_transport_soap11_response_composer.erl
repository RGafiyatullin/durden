-module(durden_transport_soap11_response_composer).
-export([get_response_envelope/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("erl_types.hrl").

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
	Envelope = ?xml:node({"Envelope", ?XML_NS_SOAP}, [], []),

	PredefinedSchemaPrefixes = ?dict_m:from_list([
		{?XML_NS_WSDL, "wsdl"},
		{?XML_NS_SOAP, "soap"},
		{?XML_NS_SOAP12, "soap12"},
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
