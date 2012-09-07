%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_handler).

-callback try_handle( Handler :: atom(), BaseUrl :: string(), Req0 :: term() ) ->
	  {halt, Req1 :: term()}
	| {next, Req1 :: term()}.
