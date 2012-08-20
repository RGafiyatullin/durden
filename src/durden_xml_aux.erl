%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_xml_aux).
-export([node/1, node/2, node/3]).
-export([imp_ns/3, attrs/2, add/2]).
-export([qname/2]).
-export([render/1]).
-export_type([xml_node/0, xml_attr/0]).

-include("app.hrl").
-include("xml.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-record(xml_node, {
	ns = undefined :: xml_ns(),
	ncname = undefined :: xml_ncname(),
	imports = ?dict_m:new() :: ?dict_t, % [ {xml_ns(), xml_ncname()} ],
	attrs = [] :: [ {xml_ncname(), string()} ],
	children = [] :: [ #xml_node{} ]
	}).
-type xml_node() :: #xml_node{}.
-type xml_attr() :: { 
						string(), 
						string() | atom() | integer() | binary() | iolist()
					}.

-spec qname( xml_ncname(), xml_ns() ) -> {qname, {xml_ns(), xml_ncname()} }.
qname(NCN, NS) -> {qname, {NS, NCN} }.

-spec node( xml_ncname() ) -> #xml_node{}.
node(NCName) ->
	#xml_node{
		ncname = NCName
	}.
-spec node( xml_ncname(), xml_ns() ) -> #xml_node{}.
node(NCName, NS) ->
	#xml_node{
		ns = NS,
		ncname = NCName
	}.

-spec node(
	QName :: { xml_ncname(), xml_ns() },
	Attrs :: [ xml_attr() ],
	Children :: [ xml_node() ]
	) -> xml_node().
node({ NCN, NS }, Attrs, Children) ->
	attrs(
		Attrs,
		add(
			Children,
			node( NCN, NS )
		)
	).

-spec imp_ns(NS :: xml_ns(), Prefix :: xml_ncname(), #xml_node{} ) -> #xml_node{}.
imp_ns( NS, Prefix, Node = #xml_node{ imports = Imports } ) ->
	Node #xml_node{
		imports = ?dict_m:store( NS, Prefix, Imports )
	}.

-spec attrs( Attrs :: [ {xml_ncname(), string()} ], Node :: #xml_node{} ) -> #xml_node{}.
attrs( NewAttrs, Node = #xml_node{ attrs = Attrs } ) ->
	Node #xml_node{
		attrs = Attrs ++ NewAttrs
	}.

-spec add( [ Child :: #xml_node{} ], Parent :: #xml_node{} ) -> NewParent :: #xml_node{}.
add( NewChildren, Parent = #xml_node{ children = Children } ) ->
	Parent #xml_node{ children = Children ++ NewChildren }.


-record(s, {
		ns = undefined,
		prefixes = ?dict_m:new()
	}).

-spec render( #xml_node{} ) -> iolist().
render( Node = #xml_node{} ) ->
	XmerlElement = to_xmerl( Node, #s{} ),
	xmerl:export([ XmerlElement ], xmerl_xml, []).

to_xmerl( _Node = #xml_node{
		ns = NodeNS,
		ncname = NodeNCN,
		attrs = Attrs,
		imports = Imports,
		children = Children
	}, 
	Ctx = #s{ 
		ns = CtxNS,
		prefixes = CtxPrefixes
	}
) ->
	NS = case NodeNS of
		undefined -> CtxNS;
		_ -> NodeNS
	end,
	Prefixes = ?dict_m:fold(
		fun( N, PNew, Ps ) ->
			?dict_m:store(N, PNew, Ps)
		end, 
		CtxPrefixes, Imports
	),
	TagName = tag_qname( NS, NodeNCN, Prefixes ),
	#xmlElement{
		name = TagName,
		attributes = render_imports( Imports ) ++ render_attrs( Attrs, Prefixes ),
		content = [
			to_xmerl(
				Child, 
				Ctx #s{
					prefixes = Prefixes,
					ns = NS
				} )
			|| Child <- Children
		]
	};
to_xmerl( Text, _Ctx = #s{} ) when is_list(Text) ->
	#xmlText{ value = Text }.

render_imports( Imports ) ->
	?dict_m:fold(
		fun( NS, Prefix, Attrs ) ->
			[
				#xmlAttribute{ name = imp_qname(Prefix), value = NS }
				| Attrs]
		end,
		[],
		Imports).

render_attribute_value({ qname, {NS, NCN} }, Prefixes) ->
	P = ?dict_m:fetch( NS, Prefixes ),
	Bound = case P of
		[] -> [];
		_ -> P ++ ":"
	end
	++ NCN,
	% io:format("Binding {~p}~p => ~p~n", [NS, NCN, Bound]),
	Bound;

render_attribute_value(V, _Prefixes) -> V.

render_attrs( Attrs, Prefixes ) ->
	lists:foldr(
		fun( {K, V}, A ) ->
			[ 
				#xmlAttribute{ 
					name = list_to_atom(K), 
					value = render_attribute_value(V, Prefixes)
				}
				| A ]
		end,
		[],
		Attrs).

imp_qname(Prefix) ->
	case Prefix of 
		[] -> list_to_atom("xmlns");
		_ -> list_to_atom( "xmlns:" ++ Prefix )
	end.

tag_qname(NS, NCN, Prefixes) ->
	% io:format("Prefixes: ~p~n", [Prefixes]),
	% io:format("Lookup: {~p}~p~n", [NS, NCN]),
	P = ?dict_m:fetch( NS, Prefixes ),
	list_to_atom( 
		case P of
			[] -> [];
			_ -> P ++ ":"
		end
		++ NCN
	).
