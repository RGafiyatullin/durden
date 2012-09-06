-module(durden_xml_decode).
-export([decode/3]).
-include("app.hrl").
-include("wsd.hrl").
-include("xml.hrl").
-include("erl_types.hrl").

-define(predefs, durden_xml_decode_predefined).

-spec decode( erlang_type_def(), term(), #wsd{} ) -> any().
-spec decode( atom(), erlang_type_def(), term(), #wsd{} ) -> any().

decode(Def, Serialized, WSD) ->
	decode( '_', Def, Serialized, WSD).

decode( '_', #et_ref{ type = {NS, NCN} }, Serialized, WSD ) ->
	case durden_wsd:find_def(NS, NCN, WSD) of
		RecordDef = #et_record{} ->
			decode( list_to_existing_atom(NCN), RecordDef, Serialized, WSD );
		Def ->
			decode( Def, Serialized, WSD )
	end;



decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_BOOL}, Serialized, _WSD ) ->
	?predefs:decode_boolean(Serialized);

decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_STR}, Serialized, _WSD ) ->
	?predefs:decode_string(Serialized);

decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_INT}, Serialized, _WSD ) ->
	?predefs:decode_int(Serialized);

decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_UUID}, Serialized, _WSD ) ->
	?predefs:decode_uuid(Serialized);

decode( RecordName, #et_record{ fields = Fields }, Serialized, WSD ) ->
	durden_xml_decode_record:decode_record( RecordName, Fields, Serialized, WSD );

decode( '_', #et_list{ type = TRef }, XmlList, WSD) when is_list(XmlList) ->
	[
		decode( TRef, XmlItem, WSD )
		|| {_, _, XmlItem} <- XmlList
	];

decode( '_', Def, Serialized, _WSD ) ->
	io:format( "Def: ~p~nSer: ~p~n~n", [Def, Serialized] ).



