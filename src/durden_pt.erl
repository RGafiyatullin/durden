%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

%%% 
%%% This parse transformation is responsible for gather the types-related info about the module to be a SOAP-handler.
%%% What it does:
%%% * find the annotations about Target-Namespace, Service-Name
%%% * find the functions to be exported (the ones specified inside -soap_exports([FuncName/Arity]) directive)
%%% * gather all the types participating in exported-functions' declarations
%%% * export the SOAP-calls
%%% * add the following callbacks:
%%%  - '#durden.get_wsd#'/0
%%%  - (cowboy_http_handler)init/3
%%%  - (cowboy_http_handler)handle/2
%%%  - (cowboy_http_handler)terminate/2
%%%

-module(durden_pt).
-export([parse_transform/2]).

-include("durden.hrl").
-include("pt.hrl").
-include("erl_types.hrl").
-include("wsd.hrl").

%%
%% Parse transformation entry point
%%
-spec parse_transform( InForms :: [ pt_form() ], PTOpts :: [ term() ] ) -> OutForms :: [ term() ].
parse_transform(Forms, _Opts) ->
	io:format("Durden parse transformation...~n"),
	{ok, TargetNS} = get_target_ns( Forms ),
	{ok, ServiceName} = get_service_name( Forms ),
	{ok, SoapExports} = get_soap_exports( Forms ),
	{ok, TypeInfo} = get_erl_types( TargetNS, Forms, SoapExports ),
	{ok, WSD} = create_wsd( ServiceName, TargetNS, SoapExports, TypeInfo ),
	
	Forms_WSD_Exported = export_wsd_calls( Forms, WSD ),
	Forms_SoapExported = export_soap_calls( Forms_WSD_Exported, SoapExports ),
	_Forms_CowboyHandlers = durden_pt_aux_cowboy:add_cowboy_handler_callbacks( Forms_SoapExported ).

-spec create_wsd(
	ServiceName :: string(), 
	TargetNS :: xml_ns(), 
	SoapExports :: [{atom(), integer()}],
	TypeInfo :: #erl_type_info{}
) ->
	#wsd{}.
create_wsd( ServiceName, TargetNS, SoapExports, TypeInfo = #erl_type_info{} ) ->
	Ctx0 = durden_wsd:new_context( ServiceName, TargetNS ),
	Ctx1 = durden_wsd:gather_types( TypeInfo, SoapExports, Ctx0 ),
	{ok, durden_wsd:finalize( Ctx1 )}.

-spec export_wsd_calls( Forms :: [pt_form()], WSD :: #wsd{} ) -> [pt_form()].
export_wsd_calls(
	Forms,
	WSD = #wsd{}
) ->
	lists:foldl(
		fun(F, Fs) ->
			durden_pt_aux_ast:add_exported_func( 
				F,
				Fs
			)
		end,
		Forms,
		[
			durden_pt_aux_ast:func_form( '#durden.get_wsd#', [], WSD )
		]
	).

-spec export_soap_calls( Forms :: [pt_form()], SoapExports :: [{atom(), integer()}] ) -> [pt_form()].
export_soap_calls( Forms, SoapExports ) ->
	lists:foldl(
		fun( {F, A}, Fs ) ->
			durden_pt_aux_ast:export_func( {F, A}, Fs )
		end,
		Forms,
		SoapExports).

-spec get_target_ns( Forms :: [ pt_form() ] ) -> { ok, xml_ns() } | { error, attr_missing }.

get_target_ns( Forms ) ->
	[ { attribute, _, soap_target_ns, TargetNS } ] = lists:filter(
			fun 
				( { attribute, _, soap_target_ns, _ } ) -> true;
				( _ ) -> false
			end,
			Forms
		),
	{ok, TargetNS}.
	
get_service_name( Forms ) ->
	[ { attribute, _, soap_service_name, ServiceName } ] = lists:filter(
			fun
				({ attribute, _, soap_service_name, _ }) -> true;
				( _ ) -> false
			end,
			Forms
		),
	{ok, ServiceName}.



-spec get_erl_types( TargetNS :: xml_ns(), Forms :: [ pt_form() ], SoapExports :: [ {atom(), integer()} ] ) -> { ok, #erl_type_info{} }.
get_erl_types( TargetNS, Forms, SoapExports ) ->
	Ctx0 = lists:foldl(
		fun( F, Ctx ) ->
			durden_erl_types:parse_form(F, Ctx)
		end,
		durden_erl_types:new_context( TargetNS ),
		Forms
	),
	Ctx1 = lists:foldl(
		fun( FA, Ctx ) ->
			durden_erl_types:process_soap_action( FA, Forms, Ctx )
		end,
		Ctx0,
		SoapExports),
	TypeInfo = durden_erl_types:finalize( Ctx1 ),
	{ok, TypeInfo}.

-spec get_soap_exports( Forms :: [ pt_form() ] ) -> {ok, [ { FuncName :: atom(), Arity :: integer() } ]}.
get_soap_exports( Forms ) ->
	Exports = get_soap_exports_loop( Forms, [] ),
	soap_exports_check_name_collisions( Exports ).

-spec soap_exports_check_name_collisions( [ { atom() , integer() } ] ) -> 
	{ ok, [ {atom(), integer()} ] } 
	| { error, term() }.

soap_exports_check_name_collisions( Exports ) ->
	FuncTimes = lists:foldl(
		fun( {F, _}, M ) ->
			NCnt = case ?dict_m:find(F, M) of
				{ok, Cnt} -> Cnt;
				error -> 0
			end,
			?dict_m:store( F, NCnt + 1, M )
		end,
		?dict_m:new(),
		Exports ),
	FuncTimesFiltered = ?dict_m:to_list(
		?dict_m:filter(
			fun
				( _, 1 ) ->
					false;
				( _F, _Times ) ->
					true
			end, FuncTimes)),
	case FuncTimesFiltered of
		[] ->
			{ok, Exports};
		[ _ | _ ] ->
			lists:foreach(
				fun({ F, T }) ->
					io:format("[ERROR] SOAP-action '~p' exported multiple times (~p)~n", [ F, T ])
				end, FuncTimesFiltered),
			{error, overloaded_soap_actions}
	end.


-spec get_soap_exports_loop( [ pt_form() ], [ {atom(), integer()} ] ) -> [ { atom(), integer() } ].

get_soap_exports_loop( [], Exports ) -> Exports;
get_soap_exports_loop( [ 
	{attribute, _, soap_actions, Actions}
	| SoFar ],
	Exports
) ->
	NExports = lists:foldl(
		fun( {F, A}, Es ) ->
			[ {F, A} | Es ]
		end,
		Exports,
		Actions),
	get_soap_exports_loop( SoFar, NExports );
get_soap_exports_loop( [ _ | SoFar], Exports ) -> get_soap_exports_loop( SoFar, Exports ).
