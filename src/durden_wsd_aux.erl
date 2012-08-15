%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsd_aux).
-export([http_uri_append/2]).
-export([resolve_ns/2]).

-include("xml.hrl").
-include("wsd.hrl").

-spec http_uri_append( string(), string()) -> string().
http_uri_append(URI, Appendix) ->
	{ok, 
		{ Scheme, UserOpts, Host, Port, Path, QS }
	} = http_uri:parse( URI ),
	NewPath = filename:join(Path, Appendix),
	
	lists:flatten([
		atom_to_list(Scheme), "://",
		case UserOpts of 
			[] -> [] ; 
			_ -> UserOpts ++ "@" 
		end,
		Host,
		case {Scheme, Port} of 
			{http, 80} -> [] ;
			{https, 443} -> [] ;
			_ -> ":" ++ integer_to_list(Port)
		end,
		NewPath,
		case QS of [] -> [] ; _ -> "?" ++ QS end
	]).


-spec resolve_ns( 
	NS_or_atom :: xml_ns() | tns | tns_records | tns_funcs,
	ServiceTargetNS :: xml_ns()
) -> xml_ns().
resolve_ns(TargetNS, NS) ->
	case NS of
		tns -> http_uri_append(TargetNS, "s0");
		tns_records -> http_uri_append(TargetNS, "s1");
		tns_funcs -> http_uri_append(TargetNS, "s2");
		_ -> NS
	end.





