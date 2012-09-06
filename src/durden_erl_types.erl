%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_erl_types).
-export([new_context/1, finalize/1, parse_form/2, process_soap_action/3]).
-export([qname/2]).

-include("durden.hrl").
-include("pt.hrl").
-include("erl_types.hrl").

-record(s, {
		target_ns :: xml_ns(),
		types = ?dict_m:new() :: ?dict_t,
		refs = ?dict_m:new() :: ?dict_t % [] :: [ { qname(), integer() } ]
	}).

-spec new_context(TargetNS :: xml_ns()) -> #s{}.
new_context( TargetNS ) -> #s{ target_ns = TargetNS }.

-spec finalize( Context :: #s{} ) -> { ok, #erl_type_info{} }.
finalize( 
	#s{
		types = Types,
		refs = Refs
	}
) ->
	#erl_type_info{
		types = Types,
		refs = Refs
	}.

-spec process_soap_action(
	Action :: { atom(), integer() },
	Forms :: [ pt_form() ],
	Context :: #s{}
) -> NewContext :: #s{}.
process_soap_action(
	{Func, Arity},
	Forms,
	Ctx = #s{
		target_ns = TargetNS,
		types = Types,
		refs = Refs
	}
) ->
	case lists:filter(
		fun
			( {attribute, _, spec, { {F, A}, _ } } ) 
				when F == Func andalso A == Arity
				-> true;
			( _ ) -> false
		end,
		Forms) 
	of
		[ _FSpec = {
			attribute, _, spec, { 
				{ _, _ },
				[
					{ type, _,'fun',
						[
							{type,_,product,
								ArgSpecs
								% [
								% 	{ann_type,0,[{var,0,'Arg0'},{type,0,any,[]}]}
								%	...
								% 	{ann_type,0,[{var,0,'ArgN'},{type,0,any,[]}]}
								% ]
							},
							RetTypeSpec
						] } ] }
		} ] ->
			{ ArgsWithDefs, ArgRefs } = lists:foldl(
				fun( 
					{ ann_type, _, [ {var, _, ArgName}, ArgTypeSpec ] },
					{ As, Rs }
				) ->
					{ ArgDef, ArgRef } = spec_to_erlang_type( TargetNS, ArgTypeSpec ),
					{ [ {ArgName, ArgDef} | As ], ArgRef ++ Rs }
				end,
				{ [], [] },
				ArgSpecs
				),
			{ RetDef, RetRefs } = spec_to_erlang_type( TargetNS, RetTypeSpec ),
			FuncDef = #et_func{
				args = lists:reverse( ArgsWithDefs ),
				ret = RetDef
			},
			FuncQName = qname(TargetNS, {function, Func}),
			FuncRefs = ArgRefs ++ RetRefs,

			report_discovered_type( FuncQName, FuncDef, FuncRefs ),
			Ctx #s{
				types = ?dict_m:store( FuncQName, FuncDef, Types ),
				refs = ?dict_m:store( FuncQName, FuncRefs, Refs )
			};
		[] ->
			io:format("[ERROR] exported SOAP action '~p/~p' does not have a -spec directive.~n", [ Func, Arity ]),
			Ctx #s{}
	end.

-spec parse_form( Form :: pt_form(), Context :: #s{} ) -> NewContext :: #s{}.

parse_form(
	{attribute,_,type,
		{{record, RecordName}, FieldSpecs, _RecordParams = [] }
	},
	Ctx = #s{
		target_ns = TargetNS,
		types = Types,
		refs = Refs
	}
) ->
	QName = qname(TargetNS, {record, RecordName}),
	{ ETDef, NRefs } = record_spec_to_erlang_type( TargetNS, FieldSpecs ),
	report_discovered_type( QName, ETDef, NRefs ),
	NRefsFiltered = lists:filter(
		fun({ RefQName, _Line }) ->
			RefQName /= QName
		end,
		NRefs),
	Ctx #s{
		types = ?dict_m:store(QName, ETDef, Types),
		refs = ?dict_m:store(QName, NRefsFiltered, Refs)
	};

parse_form(
	{attribute,_,type,
		{TypeName, TypeSpec, _TypeParams = [] }
	},
	Ctx = #s{
		target_ns = TargetNS,
		types = Types,
		refs = Refs
	}
) ->
	ResolvedTNS = durden_wsd_aux:resolve_ns( TargetNS, tns ) ,
	QName = qname(TargetNS,  TypeName ),
	case QName of
		{TNS, _} 
		when TNS == ResolvedTNS ->
			{ ETDef, NRefs } = spec_to_erlang_type( TargetNS, TypeSpec ),
			report_discovered_type( QName, ETDef, Refs ),
			Ctx #s{
				types = ?dict_m:store(QName, ETDef, Types),
				refs = ?dict_m:store(QName, NRefs, Refs)
			};
		_ ->
			Ctx #s{
				types = ?dict_m:store(QName, predefined, Types)
			}
	end;

% parse_form(
% 	{attribute,_,type,

% 		},
% 	Ctx = #s{}
% ) ->
% 	Ctx #s{};

parse_form( _F, Ctx = #s{} ) ->
	Ctx.

-spec record_spec_to_erlang_type( xml_ns(), [ term() ] ) -> { erlang_type_def(), [ erlang_type_name() ] }.
record_spec_to_erlang_type( TargetNS, FieldSpecs ) ->
	case record_spec_all_fields_are_typed( FieldSpecs ) of
		true ->
			{ FieldDefs, FieldRefs } = lists:foldl(
					fun( 
						{typed_record_field, {record_field, _, {atom, _, FieldName}}, FieldSpec},
						{ Ds, Rs }
					) ->
						{ D, R } = spec_to_erlang_type( TargetNS, FieldSpec ),
						{
							[ {FieldName, D} | Ds ],
							Rs ++ R
						}
					end,
					{ [], [] },
					FieldSpecs
				),
			{ 
				#et_record{ 
					fields = lists:reverse(FieldDefs)
				}, 
				FieldRefs
			};
		false ->
			{ untyped_fields, [] }
	end.

-spec record_spec_all_fields_are_typed( FieldSpecs :: [ term() ] ) -> boolean().
record_spec_all_fields_are_typed( [] ) -> true;
record_spec_all_fields_are_typed( [ 
	{typed_record_field,_,_}
	| SoFar ]
) -> 
	record_spec_all_fields_are_typed( SoFar );
record_spec_all_fields_are_typed( 
	[ _Argh | _ ]
) ->
	false.


-spec spec_to_erlang_type( xml_ns(), pt_form_type() ) -> { erlang_type_def(), [ { xml_qname(), integer() } ] }.

% -spec range_type() :: 1..10.
spec_to_erlang_type(
	TargetNS, 
	{type, SrcLine ,range,[
		{integer,_,LoBoundary},
		{integer,_,HiBoundary}
	]}
) ->
	{ #et_range{ lo = LoBoundary, hi = HiBoundary }, [ { qname(TargetNS, integer), SrcLine } ]};

% -spec list_type() :: [ <ItemType> ].
spec_to_erlang_type(
	TargetNS,
	{type,_,list,[
		ItemTypeSpec
	]}
) ->
	{ ItemDef, Refs } = spec_to_erlang_type( TargetNS, ItemTypeSpec ),
	{ #et_list{
		type = ItemDef
	} , Refs };

spec_to_erlang_type(
	TargetNS,
	{ type, _, union, [ {atom, _, undefined}, Other ] }
) ->
	spec_to_erlang_type(TargetNS, Other);

spec_to_erlang_type(
	TargetNS,
	{ type, _, record, [ {atom, SrcLine, RecordName} ] }
) ->
	{ #et_ref{
		type = qname(TargetNS, {record, RecordName})
	}, [ { qname(TargetNS, {record, RecordName}), SrcLine } ] };

% -spec color() :: red | green | blue | #or_may_be_even_something_odd{}.
spec_to_erlang_type(
	TargetNS,
	{ type, _, union, OptionSpecs }
) ->
	{ OptDefs, Refs } = lists:foldl(
			fun( S, { Os, Rs } ) ->
				{ D, NRs } = spec_to_erlang_type( TargetNS, S ),
				{ [ D | Os ], NRs ++ Rs }
			end,
			{ [], [] },
			OptionSpecs
		),
	{ #et_union{
		options = lists:reverse(OptDefs)
	}, Refs };

spec_to_erlang_type(
	TargetNS,
	{ atom, _, Val }
) ->
	{ #et_atom{
		value = Val
	}, [] };

% spec_to_erlang_type(
% 	{ type, _, string, _TypeParams = [] }
% ) ->
% 	{ 
% 		#et_predefined{ ns = ?XML_SCHEMA_XSD, name = "string" },
% 		[]
% 	};

% -spec t_alias() :: t().
spec_to_erlang_type(
	TargetNS,
	{ type, SrcLine, TypeName, _TypeParams = [] }
) ->
	{ #et_ref{ type = qname(TargetNS, TypeName) }, [ { qname(TargetNS, TypeName), SrcLine } ] };

spec_to_erlang_type(_TargetNS, _) -> { not_implemented, [] }.

-spec qname(TargetNS :: xml_ns(), atom() | {record, atom()} ) -> xml_qname().

qname(_TargetNS,  boolean ) -> { ?XML_NS_XSD, ?XS_TYPE_BOOL };
qname(_TargetNS,  string ) -> { ?XML_NS_XSD, ?XS_TYPE_STR };
qname(_TargetNS,  integer ) -> { ?XML_NS_XSD, ?XS_TYPE_INT };
qname(_TargetNS,  uuid ) -> { ?XML_NS_XSD, ?XS_TYPE_UUID };
qname(_TargetNS,  guid ) -> { ?XML_NS_XSD, ?XS_TYPE_UUID };
qname(_TargetNS,  date ) -> { ?XML_NS_XSD, "date" };
qname(_TargetNS,  time ) -> { ?XML_NS_XSD, "time" };
qname(_TargetNS,  datetime ) -> { ?XML_NS_XSD, "datetime" };

qname(TargetNS,  {function, FuncName} ) ->
	{ durden_wsd_aux:resolve_ns( TargetNS, tns_funcs ), atom_to_list(FuncName) };
	% { durden_wsd_aux:resolve_ns( TargetNS, tns ), atom_to_list(FuncName) };

qname(TargetNS,  {record, RecordNameAtom} ) ->
	{ durden_wsd_aux:resolve_ns( TargetNS, tns_records ), atom_to_list(RecordNameAtom) };
	% { durden_wsd_aux:resolve_ns( TargetNS, tns ), atom_to_list(RecordNameAtom) };

qname( TargetNS,  TypeAtom ) ->
	{ durden_wsd_aux:resolve_ns( TargetNS, tns ), atom_to_list(TypeAtom) }.



-spec report_discovered_type( 
	QName :: xml_qname(), 
	Def :: erlang_type_def(), 
	Refs :: [ {xml_qname(), integer()} ] 
) -> ok.

report_discovered_type(QName, Def, Refs) ->
	io:format(
		"Erlang type ~p~n"
		"\tDef:  ~p~n"
		"\tRefs: ~p~n",
		[ QName, Def, Refs ]),
	ok.
