%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_pt_aux_ast).
-export([func_form/3, add_exported_func/2, export_func/2]).

-spec func_form(FuncName :: atom(), ArgNames :: [ atom() ], RetValue :: term() ) -> term().
func_form(Name, Args, Value) ->
	{
		function, 0, Name, length(Args),
		[
			{
				clause, 0,
				[ { var, 0, VN } 
					|| VN <- Args ],
				[],
				[ erl_parse:abstract( Value ) ]
			}
		]
	}.

export_func( {F, A}, Forms ) ->
	[ File, Mod | Rest ] = Forms,
	[ 
		File, Mod,
		{attribute, 0, export, [{ F, A }]}
		| Rest
	].

-spec add_exported_func( term(), [term()] ) -> [term()].
add_exported_func( Func = { function, _, FuncName, FuncArity, _ }, Forms ) ->
	[ File, Mod | Rest ] = Forms,
	export_func( 
		{ FuncName, FuncArity }, 
		Forms ++ [
				{attribute, 0, spec, 
					{ {FuncName, FuncArity},
						[
							{ type, 0,'fun',
								[
									{type,0,product,
										lists:map(
											fun(_) ->
												{ann_type,0,[{var,0,'_'},{type,0,any,[]}]}
											end,
											lists:seq(1, FuncArity)
										)
									},
									{type,0,any,[]}
								] } ] } },
				Func
			]
		).


