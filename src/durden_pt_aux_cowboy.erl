%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

%%%
%%% Helper module to work with AST-forms concerning cowboy request handling routines
%%%

-module(durden_pt_aux_cowboy).
-export([add_cowboy_handler_callbacks/1]).

-define(aux_ast, durden_pt_aux_ast).

-spec add_cowboy_handler_callbacks( [term()] ) -> [term()].
add_cowboy_handler_callbacks( Forms ) ->
	lists:foldl(
		fun( F, Code ) ->
			?aux_ast:add_exported_func( F, Code )
		end,
		Forms, [
			?aux_ast:func_form('init', ['_', '_Req', '_Opts'], {upgrade, protocol, durden_cowboy_handler}),
			?aux_ast:func_form('handle', ['_Req', '_State'], ignore),
			?aux_ast:func_form('terminate', ['_Req', '_State'], ok)
		]).
