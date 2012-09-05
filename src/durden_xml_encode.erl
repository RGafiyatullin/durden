-module(durden_xml_encode).
-export([encode/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

-spec encode(any(), erlang_type_def(), #wsd{}) -> [ ?xml:xml_node() ].

encode(true, #et_predefined{ ns = ?XML_NS_XSD, name = "boolean"}, _WSD = #wsd{}) -> ["true"];
encode(false, #et_predefined{ ns = ?XML_NS_XSD, name = "boolean"}, _WSD = #wsd{}) -> ["false"];

encode(IntValue, #et_predefined{ ns = ?XML_NS_XSD, name = "int"}, _WSD = #wsd{}) 
	when is_integer(IntValue) 
	-> [integer_to_list(IntValue)];
encode(IntValue, #et_predefined{ ns = ?XML_NS_XSD, name = "int"}, _WSD = #wsd{}) -> throw({xml_encode_error, string, IntValue});

encode(StrValue, #et_predefined{ ns = ?XML_NS_XSD, name = "string"}, _WSD = #wsd{}) when is_list(StrValue) -> [StrValue];
encode(StrValue, #et_predefined{ ns = ?XML_NS_XSD, name = "string"}, _WSD = #wsd{}) -> throw({xml_encode_error, string, StrValue});

encode(ListOfValues, #et_list{ type = Type }, WSD = #wsd{} ) when is_list(ListOfValues) ->
	lists:foldl(
		fun(EveryValue, Acc) ->
			Acc ++ encode(EveryValue, Type, WSD)
		end,
		[],
		ListOfValues
	);

encode(
		GuidValue, 
		#et_predefined{ ns = ?XML_NS_XSD, name = "guid"}, 
		_WSD = #wsd{}
	) 
	when is_list(GuidValue) 
	andalso length(GuidValue) == 36 
	->
		[GuidValue];

encode(
		GuidValue, 
		#et_predefined{ ns = ?XML_NS_XSD, name = "guid"}, 
		_WSD = #wsd{}
	)
	->
		throw(
			{ xml_encode_error, guid, {GuidValue, is_list(GuidValue), catch length(GuidValue) } }
		);


encode(Value, #et_ref{ type = {NS, NCN} }, WSD = #wsd{}) ->
	EncodeAs = durden_wsd:find_def(NS, NCN, WSD),
	encode(Value, EncodeAs, WSD);

encode(Value, #et_integer{}, _WSD = #wsd{}) ->
	case catch {ok, integer_to_list(Value)} of
		{ok, IntAsString} ->
			[ IntAsString ];
		_Error ->
			throw({xml_encode_error, integer, Value})
	end;

encode(Value, #et_float{}, _WSD = #wsd{}) ->
	% TODO: Standard may need some other representation of floats here. Will do it later.
	case catch {ok, float_to_list(Value)} of
		{ok, FloatAsString} ->
			[ FloatAsString ];
		_Error ->
			throw({xml_encode_error, float, Value})
	end;

encode(Value, #et_atom{ value = ExpectedValue }, _WSD = #wsd{}) ->
	case Value == ExpectedValue of
		true ->
			[ atom_to_list(Value) ];
		false ->
			throw({xml_encode_error, {atom, ExpectedValue}, Value})
	end;

encode(Value, UnionDef = #et_union{ options = Options }, WSD = #wsd{}) ->
	case 
		lists:foldl(
			fun
				(_Opt, {handled, Encoded}) ->
					{handled, Encoded};
				(Opt, not_handled) ->
					case catch {ok, encode(Value, Opt, WSD)} of
						{ok, Encoded} ->
							{handled, Encoded};
						_Error ->
							not_handled
					end
			end,
			not_handled,
			Options)
	of
		{handled, Encoded} ->
			Encoded;
		not_handled ->
			throw( {xml_encode_error, UnionDef, Value} )
	end;

encode(Value, #et_string{}, _WSD = #wsd{}) when is_list(Value) ->
	[ Value ];
encode(Value, #et_string{}, _WSD = #wsd{}) ->
	throw({xml_encode_error, string, Value});

encode(Value, #et_range{ lo = Lo, hi = Hi }, WSD = #wsd{}) ->
	case {Lo =< Value, Hi >= Value} of
		{true, true} ->
			encode(Value, #et_integer{}, WSD);
		{false, _} ->
			throw({xml_encode_error, {range, Lo, Hi}, Value});
		{_, false} ->
			throw({xml_encode_error, {range, Lo, Hi}, Value})
	end;

encode(RecordTuple, #et_record{ fields = FieldDefs }, WSD = #wsd{
		target_ns = TargetNS
	}
) ->
	[ _RecordNameAtom | FieldValues ] = tuple_to_list(RecordTuple),
	RecordNS = durden_wsd_aux:resolve_ns(TargetNS, tns_records),
	% RecordName = atom_to_list( RecordNameAtom ),
	RecordFieldsWithDefs = lists:zip( FieldDefs, FieldValues ),
	FieldsRenderedReverse = lists:flatten(
		lists:foldl(
			fun({FNameAndDef, FValue}, Acc) ->
				{FName, FDef} = FNameAndDef,
				FNode = ?xml:node({atom_to_list(FName), RecordNS},
					[],
					encode( FValue, FDef, WSD )
				),
				[ FNode | Acc ]
			end,
			[],
			RecordFieldsWithDefs
		)
	),
	FieldsRendered = lists:reverse(FieldsRenderedReverse),
	% [ ?xml:node({RecordName, RecordNS}, [], FieldsRendered) ];
	FieldsRendered;

encode(Value, EncodeAs, _WSD) -> 
	io:format("error encoding ~p as ~p~n", [Value, EncodeAs]),
	throw({xml_encode_error, {not_implemented, EncodeAs}, Value}).


