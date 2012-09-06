%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_xml_encode).
-export([encode/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

-define(predefs, durden_xml_encode_predefined).

-spec encode(any(), erlang_type_def(), #wsd{}) -> [ ?xml:xml_node() ].

encode(Value, #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_BOOL}, _WSD = #wsd{}) -> ?predefs:encode_boolean(Value);
encode(Value, #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_INT}, _WSD = #wsd{}) -> ?predefs:encode_int(Value);
encode(Value, #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_STR}, _WSD = #wsd{}) -> ?predefs:encode_string(Value);
encode(Value, #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_UUID}, _WSD = #wsd{} ) -> ?predefs:encode_uuid(Value);

encode(ListOfValues, #et_list{ type = Type }, WSD = #wsd{} ) when is_list(ListOfValues) ->
	lists:foldl(
		fun(EveryValue, Acc) ->
			Acc ++ encode(EveryValue, Type, WSD)
		end,
		[],
		ListOfValues
	);

encode(Value, #et_ref{ type = {NS, NCN} }, WSD = #wsd{}) ->
	EncodeAs = durden_wsd:find_def(NS, NCN, WSD),
	encode(Value, EncodeAs, WSD);

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

encode(Value, #et_range{ lo = Lo, hi = Hi }, WSD = #wsd{}) ->
	case {Lo =< Value, Hi >= Value} of
		{true, true} ->
			encode(Value, #et_predefined{ ns = ?XML_NS_XSD, name = ?XS_TYPE_INT }, WSD);
		{false, _} ->
			throw({xml_encode_error, {range, Lo, Hi}, Value});
		{_, false} ->
			throw({xml_encode_error, {range, Lo, Hi}, Value})
	end;

encode(RecordTuple, #et_record{ fields = FieldDefs }, WSD = #wsd{}
) ->
	durden_xml_encode_record:encode_record(RecordTuple, FieldDefs, WSD);

encode(Value, EncodeAs, _WSD) -> 
	io:format("error encoding ~p as ~p~n", [Value, EncodeAs]),
	throw({xml_encode_error, {not_implemented, EncodeAs}, Value}).


