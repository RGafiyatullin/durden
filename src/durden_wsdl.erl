%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsdl).
-compile({parse_transform, gin}).
-export([module_wsdl/2, wsdl_from_wsd/2]).

-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").

-record(w, {
		types = [] :: [ { NonUnique :: xml_ns(), ?xml:xml_node() } ],
		messages = [] :: [ ?xml:xml_node() ],
		port_ops = [] :: [ { NonUnique :: xml_ncname(), ?xml:xml_node() } ],
		bind_ops = [] :: [ { NonUnique :: xml_ncname(), ?xml:xml_node() } ]
	}).


-spec module_wsdl( string(), atom() ) -> { ok, XmlWSDL :: iolist(), WSD :: #wsd{} }.
module_wsdl(HandlerURL, Handler) ->
	WSD = Handler:'#durden.get_wsd#'(),
	XmlWSDL = wsdl_from_wsd(HandlerURL, WSD),
	{ok, XmlWSDL, WSD}.

-spec wsdl_from_wsd( string(), #wsd{} ) -> iolist().
wsdl_from_wsd(
	HandlerURL,
	WSD = #wsd{
		target_ns = TargetNS,
		service_name = ServiceName,
		schemas = Schemas
	}
) ->
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
	{SchemaPrefixes, AllTypes, _} = ?dict_m:fold(
		fun( NS, SchemaTypes, { PSP, Types, Idx } ) ->
			NewTypes = ?dict_m:fold(
				fun( NCN, TypeDef, Ts ) ->
					?dict_m:store({NCN, NS}, TypeDef, Ts)
				end,
				Types,
				SchemaTypes
				),
			case ?dict_m:find( NS, PSP ) of
				{ok, _} -> { PSP, NewTypes, Idx };
				error ->
					P = "s" ++ integer_to_list(Idx),
					{ ?dict_m:store( NS, P, PSP ), NewTypes, Idx + 1 }
			end
		end,
		{ PredefinedSchemaPrefixes, ?dict_m:new(), 0 },
		Schemas
		),

	WSDL = process_types(AllTypes, SchemaPrefixes, TargetNS),
	% io:format("WSDL: ~p~n", [WSDL]),
	_XmlWSDL = render(WSDL, SchemaPrefixes, TargetNS, ServiceName, HandlerURL).

-spec process_types( 
	?dict_t, % [{ {xml_ncname(), xml_ns()}, erlang_type_def() }]
	?dict_t,  % [{ xml_ns(), xml_ncname() }]
	xml_ns()
) -> #w{}.
process_types( Types, SchemaPrefixes, TargetNS ) ->
	?dict_m:fold(
			fun({NCN, NS}, TypeDef, W) ->
				W #w{
					types = W #w.types ++ nodes_type( {NCN, NS}, TypeDef ),
					messages = W #w.messages ++ nodes_message( {NCN, NS}, TypeDef ),
					port_ops = W #w.port_ops ++ nodes_portops( NCN, TargetNS, TypeDef ),
					bind_ops = W #w.bind_ops ++ nodes_bindops( NCN, TargetNS, TypeDef )
				}
			end,
			#w{},
			Types
		).

-spec nodes_type( 
	{ xml_ncname(), xml_ns() },
	erlang_type_def()
) -> 
	[ { xml_ns(), ?xml:xml_node() } ].

nodes_type( {NCN, NS}, TypeDef ) ->
	[ {NS, Node} ||  Node <- durden_wsdl_types:node_type_or_element(TypeDef, NCN) ].

-spec nodes_message( 
	{ xml_ncname(), xml_ns()}, 
	erlang_type_def()
) -> [ ?xml:xml_node() ].
nodes_message( {NCN, NS}, TypeDef ) ->
	durden_wsdl_messages:messages( {NCN, NS}, TypeDef ).

-spec nodes_portops(
	xml_ncname(), 
	xml_ns(),
	erlang_type_def()
) -> [ ?xml:xml_node() ].
nodes_portops( NCN, TargetNS, TypeDef ) ->
	durden_wsdl_portops:operations( NCN, TargetNS, TypeDef ).

-spec nodes_bindops(
	xml_ncname(), 
	xml_ns(), 
	erlang_type_def()
) -> [ ?xml:xml_node() ].
nodes_bindops( NCN, TargetNS, TypeDef ) ->
	durden_wsdl_bindops:operations( NCN, TargetNS, TypeDef ).




-spec render(
	#w{},
	?dict_t,
	xml_ns(),
	string(),
	string()
) -> iolist().
render(#w{
		types = Types,
		messages = Messages,
		port_ops = PortOps,
		bind_ops = BindOps
	},
	NSPrefixes,
	TargetNS,
	ServiceName,
	HandlerURL
	) -> 
	Definitions = ?xml:node({"definitions", ?XML_NS_WSDL},
		[
			{"targetNamespace", TargetNS}
		],
		lists:flatten([
			?xml:node({"types", ?XML_NS_WSDL},[], render_schemas( Types ) ),
			Messages,
			render_port_types(PortOps, ServiceName),
			render_bindings(BindOps, ServiceName, TargetNS),
			render_service(ServiceName, TargetNS, BindOps, HandlerURL)
		])
	),
	DefinitionsNSsImported = lists:foldl(
		fun({NS, Prefix}, Defs) ->
			?xml:imp_ns(NS, Prefix, Defs)
		end,
		Definitions,
		?dict_m:to_list(NSPrefixes)
	),
	?xml:render(DefinitionsNSsImported).


render_service(ServiceName, TargetNS, BindOps, HandlerURL) ->
	AvailableBindings = ?dict_m:fetch_keys( ?dict_m:from_list( BindOps ) ),
	?xml:node(
		{"service", ?XML_NS_WSDL},
		[{"name", ServiceName}],
		[
			render_service_port(BindingType, ServiceName, TargetNS, HandlerURL)
			|| BindingType <- AvailableBindings
		]).

render_service_port(BindingType, ServiceName, TargetNS, HandlerURL) ->
	PortName = case BindingType of
		soap -> ServiceName ++ ?WSDL_SUFFIX_PT_SOAP;
		soap12 -> ServiceName ++ ?WSDL_SUFFIX_PT_SOAP12;
		http_get -> ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_GET;
		http_post -> ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_POST
	end,
	AddrType = case BindingType of
		soap -> {"address", ?XML_NS_SOAP};
		soap12 -> {"address", ?XML_NS_SOAP12};
		http_get -> {"address", ?XML_NS_HTTP};
		http_post -> {"address", ?XML_NS_HTTP}
	end,
	?xml:node(
		{"port", ?XML_NS_WSDL},
		[
			{"name", PortName },
			{"binding", ?xml:qname(PortName, TargetNS)}
		],
		[
			?xml:node(AddrType, [{"location", HandlerURL}],[])
		]
	).

render_schemas( Types ) ->
	SchemasDict = lists:foldl(
		fun({SchemaNS, TypeElement}, Schemas) ->
			SchemaTypes = case ?dict_m:find( SchemaNS, Schemas ) of
				{ok, Found} -> Found;
				error -> []
			end,
			?dict_m:store(
				SchemaNS, 
				[ TypeElement | SchemaTypes ],
				Schemas
			)
		end,
		?dict_m:new(),
		Types
		),
	[
		render_schema_node(SchemaNS, SchemaTypesList, ?dict_m:fetch_keys(SchemasDict))
		|| {SchemaNS, SchemaTypesList} <- ?dict_m:to_list(SchemasDict)
	].

render_schema_node( SchemaNS, TypesList, OtherNSs ) ->
	?xml:imp_ns(
		?XML_NS_XSD,
		"xs",
		?xml:node({"schema", ?XML_NS_XSD}, [{"targetNamespace", SchemaNS}],
			[
				?xml:node({"import", ?XML_NS_XSD}, [{"namespace", ImpNS}], [])
				|| ImpNS <- OtherNSs, ImpNS /= SchemaNS
			]
			++
			TypesList
		)
	).

render_new_port_type_node(soap, ServiceName) -> 
	?xml:node({"portType", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_SOAP}
		], []);

render_new_port_type_node(soap12, ServiceName) -> 
	?xml:node({"portType", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_SOAP12}
		], []);

render_new_port_type_node(http_get, ServiceName) ->
	?xml:node({"portType", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_GET}
		], []);

render_new_port_type_node(http_post, ServiceName) ->
	?xml:node({"portType", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_POST}
		], []).
	

render_port_types(PortOps, ServiceName) ->
	PortTypesDict = lists:foldl(
		fun( {PortTypeAtom, PortOpElement}, PortTypes ) ->
			PortTypeElement = 
				case ?dict_m:find( PortTypeAtom, PortTypes ) of
					error -> render_new_port_type_node(PortTypeAtom, ServiceName);
					{ok, Found} -> Found
				end,
			NewPortTypeElement = ?xml:add(
				[ PortOpElement ],
				PortTypeElement
			),
			?dict_m:store(PortTypeAtom, NewPortTypeElement, PortTypes)
		end,
		?dict_m:new(),
		PortOps
	),
	?dict_m:fold(
		fun(_, PortTypeElement, PortTypeElements) ->
			[ PortTypeElement | PortTypeElements ]
		end,
		[],
		PortTypesDict
	).

render_new_binding_node(soap, ServiceName, TargetNS) ->
	?xml:node({"binding", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_SOAP},
			{"type", ?xml:qname(ServiceName ++ ?WSDL_SUFFIX_PT_SOAP, TargetNS)}
		], [
			?xml:node({"binding", ?XML_NS_SOAP}, [{"transport", ?XML_NS_SOAP_TRANSPORT}], [])
		]);
render_new_binding_node(soap12, ServiceName, TargetNS) ->
	?xml:node({"binding", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_SOAP12},
			{"type", ?xml:qname(ServiceName ++ ?WSDL_SUFFIX_PT_SOAP12, TargetNS)}
		], [
			?xml:node({"binding", ?XML_NS_SOAP12}, [{"transport", ?XML_NS_SOAP_TRANSPORT}], [])
		]);
render_new_binding_node(http_get, ServiceName, TargetNS) ->
	?xml:node({"binding", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_GET},
			{"type", ?xml:qname(ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_GET, TargetNS)}
		], [
			?xml:node({"binding", ?XML_NS_HTTP}, [{"verb", "GET"}], [])
		]);
render_new_binding_node(http_post, ServiceName, TargetNS) ->
	?xml:node({"binding", ?XML_NS_WSDL}, [
			{"name", ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_POST},
			{"type", ?xml:qname(ServiceName ++ ?WSDL_SUFFIX_PT_HTTP_POST, TargetNS)}
		], [
			?xml:node({"binding", ?XML_NS_HTTP}, [{"verb", "POST"}], [])
		]).


render_bindings(BindOps, ServiceName, TargetNS) ->
	BindingsDict = lists:foldl(
		fun( {BindingAtom, BindOpElement}, Bindings ) ->
			BindingElement = 
				case ?dict_m:find( BindingAtom, Bindings ) of
					error -> render_new_binding_node(BindingAtom, ServiceName, TargetNS);
					{ok, Found} -> Found
				end,
			NewBindingElement = ?xml:add(
				[ BindOpElement ],
				BindingElement
			),
			?dict_m:store(BindingAtom, NewBindingElement, Bindings)
		end,
		?dict_m:new(),
		BindOps
	),
	?dict_m:fold(
		fun(_, BindingElement, BindingElements) ->
			[ BindingElement | BindingElements ]
		end,
		[],
		BindingsDict
	).

