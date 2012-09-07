%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsd).
-export([new_context/2, gather_types/3, finalize/1]).
-export([find_def/3]).

-include("app.hrl").
-include("xml.hrl").
-include("erl_types.hrl").
-include("wsd.hrl").

-record(s, {
	service_name :: string(),
	target_ns :: xml_ns(),
	schemas = ?dict_m:new() :: ?dict_t
	}).

-spec new_context( ServiceName :: string(), TargetNS :: xml_ns() ) -> #s{}.
new_context( ServiceName, TargetNS ) -> #s{ service_name = ServiceName, target_ns = TargetNS }.

-spec finalize( Context :: #s{} ) -> #wsd{}.
finalize( Ctx = #s{
		service_name = ServiceName,
		target_ns = TargetNS,
		schemas = Schemas
	} ) ->
	_WsdAliasesResolved = durden_wsd_alias_resolve:resolve(
		#wsd{
			service_name = ServiceName,
			target_ns = TargetNS,
			schemas = Schemas
		}
	). 

-spec find_def( 
	NS :: xml_ns(), 
	NCN :: xml_ncname(), 
	WSD :: #wsd{} 
) -> erlang_type_def() | {predefined, xml_ns(), xml_ncname()}.

find_def( NS, NCN, _WSD = #wsd{ schemas = Schemas } ) ->
	case ?dict_m:find(NS, Schemas) of
		{ok, Schema} ->
			case ?dict_m:find(NCN, Schema) of
				{ok, predefined} -> 
					#et_predefined{
						ns = NS,
						name = NCN
					};
				{ok, Def} -> Def;
				error -> 
					#et_predefined{
						ns = NS,
						name = NCN
					}
			end;
		error ->
			#et_predefined{
				ns = NS,
				name = NCN
			}
	end.


-spec gather_types( 
	TypeInfo :: #erl_type_info{},
	SoapExports :: [ { atom(), integer() } ],
	Context :: #s{}
) -> #s{}.

gather_types(
	#erl_type_info{
		types = AllTypes,
		refs = AllRefs
	},
	SoapExports,
	Context = #s{
		target_ns = TargetNS
	}
) ->
	TypesUsed = lists:foldl(
		fun( {F, _}, TU ) ->
			FQN = durden_erl_types:qname(TargetNS, {function, F}),
			gather_types_starting_with( FQN, AllRefs, AllTypes, TU, ?dict_m:new() )
		end,
		?dict_m:new(),
		SoapExports),
	TypesGrouppedByNS = group_types_by_ns( TypesUsed ),
	Context #s{
		schemas = TypesGrouppedByNS
	}.

should_halt( QN, Dict ) ->
	case ?dict_m:find(QN, Dict) of
		{ok, true} -> true;
		error -> false;
		{ok, false} -> false
	end.
set_should_halt(QN, Dict) -> ?dict_m:store(QN, true, Dict).

-spec group_types_by_ns( ?dict_t ) -> ?dict_t .
group_types_by_ns( TypesUsed ) ->
	_TypesGrouppedByNS = ?dict_m:fold(
		fun({ NS, N }, TD, Schemas) ->
			S = case ?dict_m:find(NS, Schemas) of
				error -> ?dict_m:new();
				{ok, Sch} -> Sch
			end,
			NewS = ?dict_m:store( N, TD, S ),
			?dict_m:store(NS, NewS, Schemas)
		end,
		?dict_m:new(),
		TypesUsed).

-spec gather_types_starting_with( xml_qname(), ?dict_t, ?dict_t, ?dict_t, ?dict_t ) -> ?dict_t .
gather_types_starting_with( QN, AllRefs, AllTypes, TypesUsed, Halts ) ->
	case should_halt(QN, Halts) of
		false ->
			TU1 = store_own_type( QN, AllTypes, TypesUsed ),
			TU2 = store_direct_refs( QN, AllRefs, AllTypes, TU1 ),
			_TU3 = store_indirect_refs( QN, AllRefs, AllTypes, TU2, set_should_halt(QN, Halts) );
		true ->
			TypesUsed
	end.

store_own_type( QN, AllTypes, TU ) ->
	case ?dict_m:find( QN, TU ) of
		{ok, _} -> TU;
		error ->
			case ?dict_m:find(QN, AllTypes) of
				{ ok, TD } -> ?dict_m:store( QN, TD, TU );
				error -> ?dict_m:store( QN, predefined, TU )
			end
	end.

get_direct_refs(QN, AllRefs) ->
	Ret = case ?dict_m:find(QN, AllRefs) of
		{ok, DirectRefs} -> lists:map(
			fun({RQN, _}) ->
				RQN
			end,
			DirectRefs);
		error -> []
	end,
	Ret.

store_direct_refs( QN, AllRefs, AllTypes, TU0 ) ->
	DirectRefs = get_direct_refs(QN, AllRefs),
	lists:foldl(
		fun( RQN, TU ) ->
			store_own_type( RQN, AllTypes, TU )
		end,
		TU0,
		DirectRefs).

store_indirect_refs( QN, AllRefs, AllTypes, TU0, Halts ) ->
	DirectRefs = get_direct_refs(QN, AllRefs),
	lists:foldl(
		fun( RQN, TU ) ->
			gather_types_starting_with( RQN, AllRefs, AllTypes, TU, Halts )
		end,
		TU0,
		DirectRefs).
