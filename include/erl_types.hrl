%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(durden_erl_types_hrl).
-define(durden_erl_types_hrl, included).

-include("app.hrl").
-include("xml.hrl").

-define(XS_TYPE_INT, "int").
-define(XS_TYPE_BOOL, "boolean").
-define(XS_TYPE_STR, "string").
-define(XS_TYPE_UUID, "guid").

-record( et_ref, {
		type :: atom()
	} ).
-record( et_atom, {
		value :: atom()
	} ).
% -record( et_integer, {
% 		name = integer
% 			:: integer
% 			 | int8
% 			 | int16
% 			 | int32
% 			 | int64
% 	} ).
% -record( et_float, {
% 		name = single
% 			:: single
% 			 | double % TODO: check how erlang works with those
% 	}).

-record( et_binary, {
		enc = hex
			:: hex
			 | base64
	} ).

% TODO: add datetimes

-record( et_union, {
		options = []
			:: [ erlang_type_def() ]
	} ).

-record( et_list, {
		type = undefined
			:: erlang_type_def()
	} ).

-record( et_range, {
		lo :: integer(),
		hi :: integer()
	} ).

-record( et_record, {
		fields = [] :: [ { atom(), erlang_type_def() } ]
	} ).

-record( et_predefined, {
		ns :: xml_ns(),
		name :: xml_ncname()
	} ).

-record( et_func, {
		args = [] :: [ { atom(), erlang_type_def() } ],
		ret :: erlang_type_def()
	} ).

-type erlang_type_def()
	:: #et_binary{}
	 | #et_union{}
	 | #et_list{}
	 | #et_range{}
	 | #et_record{}
	 % | #et_integer{}
	 % | #et_float{}
	 
	 | #et_ref{}
	 | #et_predefined{}

	 | #et_func{}
	.
-type erlang_type_name() :: atom().

-record(erl_type_info, {
		types :: ?dict_t,
		refs :: ?dict_t
	}).

-endif. % durden_erl_types_hrl