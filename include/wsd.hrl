%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(durden_wsd_hrl).
-define(durden_wsd_hrl, included).

-include("xml.hrl").
-include("erl_types.hrl").

-record(schema, {
	target_ns :: xml_ns(),
	namespaces :: [ { xml_ns(), xml_ncname() } ],
	types :: [ { xml_ncname(), erlang_type_def() } ]
	}).

-record(wsd, {
	service_name :: string(),
	target_ns :: xml_ns(),
	schemas :: [ #schema{} ]
	}).

-endif. % durden_wsd_hrl
