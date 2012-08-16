%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsdl_types).
-compile({parse_transform, gin}).
-export([ node_type_or_element/2, name_attr/1 ]).

-include("app.hrl").
-include("erl_types.hrl").
-include("wsdl.hrl").

-spec node_type_or_element(
		erlang_type_def(),
		TName :: undefined | string()
	) -> [ durden_xml_aux:xml_node() ].
-spec name_attr( undefined | string() ) -> [ durden_xml_aux:xml_attr() ].

node_type_or_element( #et_union{
	options = Options
}, TName) ->
	[
		?xml:node({"simpleType", ?XML_NS_XSD},
				name_attr(TName),
				[
					?xml:node({"restriction", ?XML_NS_XSD},
						[ { "base", qname("string", ?XML_NS_XSD) } ],
						[
							?xml:node({"enumeration", ?XML_NS_XSD},
								[ {"value", Opt} ],[])
							|| #et_atom{ value = Opt } <- Options
						]
					)
				]
			)
	];

node_type_or_element( #et_range{ lo = Lo, hi = Hi }, TName ) ->
	[
		?xml:node({"simpleType", ?XML_NS_XSD},
				name_attr(TName),
				[
					?xml:node({"restriction", ?XML_NS_XSD},
						[ {"base", qname(?XS_TYPE_INT, ?XML_NS_XSD) } ],
						[
							?xml:node({"minInclusive", ?XML_NS_XSD}, [{"value", Lo}], []),
							?xml:node({"maxInclusive", ?XML_NS_XSD}, [{"value", Hi}], [])
						]
					)
				]
			)
	];

node_type_or_element( #et_record{ fields = Fields }, TName ) ->
	[
		?xml:node({"complexType", ?XML_NS_XSD},
			name_attr(TName),
			[
				?xml:node({"sequence", ?XML_NS_XSD},
					[],
					[
						?xml:node({"element", ?XML_NS_XSD}, 
							[
								{"name", FName},
								{"type", {qname, type_node_extract_type(FType) } }, % TODO: This is not okay! Twice
								{"minOccurs", 1},
								{"maxOccurs", 1}
							],
							[]
						)
						|| 
						{FName, FType} <- Fields
					]
				)
			]
		)
	];

node_type_or_element( #et_func{
		args = Args,
		ret = Ret
	}, TName ) ->
		[
			?xml:attrs(
				name_attr(TName),
				?xml:add(lists:flatten([
					node_type_or_element(#et_record{
						fields = Args
						}, undefined)
					]),
					?xml:node("element", ?XML_NS_XSD)
				)
			),
			?xml:attrs(
				name_attr(TName ++ ?WSDL_SUFFIX_RESPONSE),
				?xml:add(lists:flatten([
						node_type_or_element(#et_record{
							fields = [
								{?WSDL_FIELD_RESULT(TName), Ret}
							]
						}, undefined)
					]),
					?xml:node("element", ?XML_NS_XSD)
				)
			)
		];

node_type_or_element( predefined, "guid" ) -> [];
	% <xs:simpleType name="guid">
	% 	<xs:restriction base="xs:string">
	% 		<xs:pattern value="[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"/>
	% 	</xs:restriction>
	% </xs:simpleType>
	% Pattern = ?xml:node(
	% 	{"pattern", ?XML_NS_XSD},
	% 	[
	% 		{"value", "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"}
	% 	],
	% 	[]
	% ),
	% Restriction = ?xml:node(
	% 	{"restriction", ?XML_NS_XSD},
	% 	[ {"base", qname("string", ?XML_NS_XSD)} ],
	% 	[ Pattern ]
	% ),
	% [ _SimpleType = ?xml:node(
	% 	{"simpleType", ?XML_NS_XSD},
	% 	name_attr("guid"),
	% 	[ Restriction ]
	% ) ];

node_type_or_element( predefined, TName ) 
	when in(TName, ["string", "integer", "date", "time", "datetime", "int", "guid", "boolean"])
->
	[];

node_type_or_element( 
	#et_list{ 
		type = #et_ref{ type = {ItemNS, ItemNCN} }
	}, TName 
) ->
	[
		?xml:node({"complexType", ?XML_NS_XSD},
			name_attr(TName),
			[
				?xml:node({"sequence", ?XML_NS_XSD}, [], [
						?xml:node({"element", ?XML_NS_XSD}, [
								{"minOccurs", 0},
								{"maxOccurs", "unbounded"},
								% {"nillable", "true"},
								{"name", "item"},
								{"type", qname(ItemNCN, ItemNS)}
							], [])
					])
			])
	];

node_type_or_element( _TDef, TName ) -> 
	% io:format("def ~p :: ~p~n", [TName, TDef]),
	[ ?xml:node(
		{"unimplementedType", ?XML_NS_XSD},
		name_attr(TName), []
		) ].


name_attr(undefined) -> [];
name_attr(TName) -> [ {"name", TName} ].

qname( NCN, NS ) ->
	{qname, {NS, NCN} }.

type_node_extract_type( #et_ref{ type = Type } ) ->
	% io:format("Extracting ~p form et_ref~n", [Type]),
	Type;

% TODO: #et_list handled inappropriately. Should create SOAP-ENC:ArrayType
type_node_extract_type( #et_list{ type = Type } ) ->
	% io:format("Extracting ~p form et_list~n", [Type]),
	type_node_extract_type( Type ).



