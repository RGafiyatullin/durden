%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(durden_predefined_types_hrl).
-define(durden_predefined_types_hrl, included).

-type uuid() :: string().
-type guid() :: uuid().

-type time() :: { 0..23, 0..59, 0..59 }.
-type date() :: { 
		integer(),  % Live long and prosper human nation
		1..12,
		1..31
	}.
-type datetime() :: { date(), time() }.

-endif. % durden_predefined_types_hrl
