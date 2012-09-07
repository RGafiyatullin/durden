%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_xml_decode_record).

-export([
	decode_record/4
	]).

-type wsd() :: term(). % do not want to know what it means here.
-spec decode_record( atom(), list(), term(), wsd() ) -> tuple().

decode_record( _RecordName, _Fields, xml_value_omitted, _WSD ) -> undefined;

decode_record( RecordName, Fields, Serialized, WSD ) ->
	{Recognized, []} = lists:foldl(
		fun decode_record_fields_match_folder/2,
		{ [], Serialized },
		Fields),
	RecordAsList = lists:reverse(
		lists:foldr(
			fun({Def, Xml}, FieldsDecoded) ->
				[ durden_xml_decode:decode( Def, Xml, WSD ) | FieldsDecoded ]
			end,
			[ RecordName ],
			Recognized
		)
	),
	list_to_tuple(RecordAsList).

decode_record_fields_match_folder( {FName, FDef}, {Mapped, NotMapped} ) ->
	FNameStr = atom_to_list(FName),
	case [ FC || { FN, _, FC } <- NotMapped, FN == FNameStr ] of
		[ FXml ] ->
			Rest = [ F || F = { FN, _, _ } <- NotMapped, FN /= FNameStr ],
			{[ {FDef, FXml} | Mapped ], Rest };
		[] ->
			{[ {FDef, xml_value_omitted} | Mapped ], NotMapped}
	end.

