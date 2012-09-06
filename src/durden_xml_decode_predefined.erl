%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_xml_decode_predefined).

-export([
	decode_boolean/1,
	decode_string/1,
	decode_int/1,
	decode_uuid/1
	]).

-include("predefined_types.hrl").

-spec decode_boolean(term()) -> boolean().
-spec decode_string(term()) -> string().
-spec decode_int(term()) -> integer().
-spec decode_uuid(term()) -> uuid().

decode_boolean( [ "true" ] ) -> true;
decode_boolean( [ "false" ] ) -> false;
decode_boolean( xml_value_omitted ) -> false;
decode_boolean( Serialized ) -> throw({xml_decode_error, {boolean, Serialized}}).

decode_string( [ StringValue ] ) when is_list(StringValue) -> StringValue;
decode_string( xml_value_omitted ) -> "";
decode_string( Serialized ) -> throw({xml_decode_error, {string, Serialized}}).

decode_int([ IntAsString ]) when is_list(IntAsString) ->
	case catch list_to_integer(IntAsString) of
		IntValue when is_integer(IntValue) -> IntValue;
		_ -> throw({xml_decode_error, {integer, [ IntAsString ]}})
	end;
decode_int( xml_value_omitted ) -> 0;
decode_int(Serialized) ->
	throw({xml_decode_error, {integer, Serialized}}).

decode_uuid([ UUID ]) when is_list(UUID) ->
	UUID;
decode_uuid( xml_value_omitted ) -> "00000000-0000-0000-0000-000000000000";
decode_uuid(Serialized) ->
 	throw({xml_decode_error, {uuid, Serialized}}).
