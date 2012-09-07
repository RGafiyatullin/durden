-module(durden_wsd_alias_resolve).
-export([resolve/1]).

-include("app.hrl").
-include("xml.hrl").
-include("erl_types.hrl").
-include("wsd.hrl").

-spec resolve( #wsd{} ) -> #wsd{}.
resolve( WSD0 = #wsd{ schemas = Schemas } ) -> 
	AliasResolutions = ?dict_m:fold(
		fun(NS, Types, AliasTypes) ->
			?dict_m:fold( fun( NCN, _TDef, ATs ) ->
				traverse_all_types_folder( NS, NCN, WSD0, ATs )
			end, AliasTypes, Types )
		end, [], Schemas),
	WSDAliasesReplaced = lists:foldl(
		fun use_alias_resolution/2,
		WSD0,
		AliasResolutions
	).

traverse_all_types_folder( NS, NCN, WSD, ATs ) ->
	case check_type( NS, NCN, WSD ) of
		keep -> 
			ATs;
		{replace, {ReplNS, ReplNCN} } ->
			[ {NS, NCN, ReplNS, ReplNCN} | ATs ]
	end.


check_type( NS, NCN, WSD ) ->
	case durden_wsd:find_def( NS, NCN, WSD ) of
		#et_ref{ type = {ReferringToNS, ReferringToNCN} } ->
			case check_type( ReferringToNS, ReferringToNCN, WSD ) of
				keep ->
					{replace, {ReferringToNS, ReferringToNCN} };
				ReplaceIndirect = {replace, {_, _}} ->
					ReplaceIndirect
			end;
		_ ->
			keep
	end.


use_alias_resolution(Resolution = {NS0, NCN0, NS1, NCN1}, WSD0 = #wsd{ schemas = Schemas0 }) ->
	WSD0 #wsd{ schemas = 
		?dict_m:fold(
			fun (NS, Types0, Schemas) ->
				Types1 = ?dict_m:fold(
					fun
						(NCN, _TDef, Types) when {NS, NCN} == {NS0, NCN0} -> Types;
						(NCN, TDef0, Types) -> 
							TDef1 = use_alias_resolution_in_tdef( Resolution, TDef0 ),
							?dict_m:store(NCN, TDef1, Types)
					end, ?dict_m:new(), Types0
				),
				?dict_m:store(NS, Types1, Schemas)
			end, ?dict_m:new(), Schemas0)
	}.

use_alias_resolution_in_tdef( Resolution, TDef = #et_record{ fields = Fields } ) ->
	TDef #et_record{
		fields = [
			{FN, use_alias_resolution_in_tdef(Resolution, FDef)}
			|| {FN, FDef} <- Fields
		]
	};
use_alias_resolution_in_tdef( {NS0, NCN0, NS1, NCN1}, TDef = #et_list{ type = {NS0, NCN0} } ) ->
	TDef #et_list{ type = {NS1, NCN1} };
use_alias_resolution_in_tdef( {NS0, NCN0, NS1, NCN1}, TDef = #et_ref{ type = {NS0, NCN0} } ) -> 
	TDef #et_ref{ type = {NS1, NCN1} };
use_alias_resolution_in_tdef( Resolution, TDef ) ->
	TDef.