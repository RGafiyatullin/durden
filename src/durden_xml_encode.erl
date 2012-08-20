-module(durden_xml_encode).
-export([encode/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

-spec encode(any(), erlang_type_def(), #wsd{}) -> [ ?xml:xml_node() ].
encode(Value, #et_ref{ type = {NS, NCN} }, WSD = #wsd{}) ->
	EncodeAs = durden_wsd:find_def(NS, NCN, WSD),
	encode(Value, EncodeAs, WSD);

encode(Value, #et_integer{}, _WSD = #wsd{}) ->
	case catch {ok, integer_to_list(Value)} of
		{ok, IntAsString} ->
			[ IntAsString ];
		Error ->
			throw({xml_encode_error, integer, Value})
	end;

encode(Value, #et_float{}, _WSD = #wsd{}) ->
	% TODO: Standard may need some other representation of floats here. Will do it later.
	case catch {ok, float_to_list(Value)} of
		{ok, FloatAsString} ->
			[ FloatAsString ];
		Error ->
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

encode(Value, EncodeAs, _WSD) -> 
	io:format("error encoding ~p as ~p~n", [Value, EncodeAs]),
	throw({xml_encode_error, {not_implemented, EncodeAs}, Value}).
