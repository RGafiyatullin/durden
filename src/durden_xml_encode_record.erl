%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_xml_encode_record).

-export([
	encode_record/3
	]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").


encode_record(undefined, _FieldDefs, #wsd{}) -> [];
encode_record(RecordTuple, FieldDefs, WSD = #wsd{
		target_ns = TargetNS
	}
) ->
	[ _RecordNameAtom | FieldValues ] = tuple_to_list(RecordTuple),
	RecordNS = durden_wsd_aux:resolve_ns(TargetNS, tns_records),
	RecordFieldsWithDefs = lists:zip( FieldDefs, FieldValues ),
	FieldsRenderedReverse = lists:flatten(
		lists:foldl(
			fun({FNameAndDef, FValue}, Acc) ->
				{FName, FDef} = FNameAndDef,
				FNode = ?xml:node({atom_to_list(FName), RecordNS},
					[],
					durden_xml_encode:encode( FValue, FDef, WSD )
				),
				[ FNode | Acc ]
			end,
			[],
			RecordFieldsWithDefs
		)
	),
	_FieldsRendered = lists:reverse(FieldsRenderedReverse).