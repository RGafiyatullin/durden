-module(durden_xml_encode_predefined).
-export([
		encode_string/1,
		encode_boolean/1,
		encode_uuid/1,
		encode_int/1
	]).

-include("predefined_types.hrl").

-define(enc_err(T, V), throw({xml_encode_error, T, V})).

-spec encode_string( string() ) -> term().
encode_string( undefined ) -> "";
encode_string( Value ) when is_list(Value) -> [ Value ];
encode_string( Value ) -> ?enc_err(string, Value).

-spec encode_boolean( boolean() ) -> term().
encode_boolean( undefined ) -> encode_boolean( false );
encode_boolean( false ) -> [ "false" ];
encode_boolean( true ) -> [ "true" ];
encode_boolean( Value ) -> ?enc_err(boolean, Value).

-spec encode_uuid( uuid() ) -> term().
encode_uuid( undefined ) -> [ "00000000-0000-0000-0000-000000000000" ];
encode_uuid( Value ) when is_list(Value) andalso length(Value) == 36 -> [ Value ];
encode_uuid( Value ) -> ?enc_err(uuid, {Value, is_list(Value), catch length(Value) }).

-spec encode_int( integer() ) -> term().
encode_int(undefined) -> [ "0" ];
encode_int(Value) when is_integer(Value) -> [ integer_to_list(Value) ];
encode_int(Value) -> ?enc_err(integer, Value).
