-module(durden_xml_decode).
-export([decode/3]).
-include("app.hrl").
-include("wsd.hrl").
-include("xml.hrl").
-include("erl_types.hrl").

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



decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "boolean"}, ["true"], _WSD ) ->
	true;
decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "boolean"}, ["false"], _WSD ) ->
	false;
decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "boolean"}, Serialized, _WSD ) ->
	throw({xml_decode_error, {boolean, Serialized}});



decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "string"}, [ StringValue ], _WSD ) when is_list(StringValue) ->
	StringValue;
decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "string"}, Serialized, _WSD ) ->
	throw({xml_decode_error, {string, Serialized}});



decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_INT}, [ IntAsString ], _WSD ) when is_list(IntAsString) ->
	case catch list_to_integer(IntAsString) of
		IntValue when is_integer(IntValue) -> IntValue;
		_ -> throw({xml_decode_error, {integer, [ IntAsString ]}})
	end;
decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_INT}, Serialized, _WSD ) ->
	throw({xml_decode_error, {integer, Serialized}});



decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "guid"}, [ UUID ], _WSD ) when is_list(UUID) ->
	UUID;
decode( '_', #et_predefined{ ns = ?XML_NS_XSD, name = "guid"}, Serialized, _WSD ) ->
	throw({xml_decode_error, {uuid, Serialized}});



decode( RecordName, #et_record{ fields = Fields }, Serialized, WSD ) ->
	{Recognized, []} = lists:foldl(
		fun({FName, FDef}, {Mapped, NotMapped}) ->
			io:format("FieldName: ~p~n", [FName]),
			FNameStr = atom_to_list(FName),
			[ FXml ] = [ FC || { FN, _, FC } <- NotMapped, FN == FNameStr ],
			Rest = [ F || F = { FN, _, _ } <- NotMapped, FN /= FNameStr ],
			{[ {FDef, FXml} | Mapped ], Rest }
		end,
		{ [], Serialized },
		Fields),
	
	RecordAsList = lists:reverse(
		lists:foldr(
			fun({Def, Xml}, FieldsDecoded) ->
				[ decode( Def, Xml, WSD ) | FieldsDecoded ]
			end,
			[ RecordName ],
			Recognized
		)
	),
	list_to_tuple(RecordAsList);



decode( '_', #et_list{ type = TRef }, XmlList, WSD) when is_list(XmlList) ->
	[
		decode( TRef, XmlItem, WSD )
		|| {_, _, XmlItem} <- XmlList
	];



decode( '_', Def, Serialized, _WSD ) ->
	io:format( "Def: ~p~nSer: ~p~n~n", [Def, Serialized] ).




