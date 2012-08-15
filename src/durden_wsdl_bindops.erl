%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsdl_bindops).
-compile({parse_transform, gin}).
-export([operations/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

binding_types() -> [ soap, soap12, http_get, http_post ].

operations( NCN, TargetNS, #et_func{} ) ->
	operations(NCN, TargetNS);
operations( _, _, _ ) -> [].

operations(NCN, TargetNS) ->
	lists:flatten([
		operation(NCN, TargetNS, BindingType)
		|| BindingType <- binding_types()
	]).

operation(NCN, TargetNS, soap) -> [
	{
		soap,
		?xml:node({"operation", ?XML_NS_WSDL},
			[{"name", NCN}],
			[
				?xml:node({"operation", ?XML_NS_SOAP},
					[
						{"soapAction", durden_wsd_aux:http_uri_append(TargetNS, NCN) },
						{"style", "document"}
					], []),
				?xml:node({"input", ?XML_NS_WSDL},
					[],[
						?xml:node({"body", ?XML_NS_SOAP},[
								{"use", "literal"}
							],[])
					]),
				?xml:node({"output", ?XML_NS_WSDL},
					[],[
						?xml:node({"body", ?XML_NS_SOAP},[
								{"use", "literal"}
							],[])
					])
			])
	}
];

operation(NCN, TargetNS, soap12) -> [
	{
		soap12,
		?xml:node({"operation", ?XML_NS_WSDL},
			[{"name", NCN}],
			[
				?xml:node({"operation", ?XML_NS_SOAP12},
					[
						{"soapAction", durden_wsd_aux:http_uri_append(TargetNS, NCN) },
						{"style", "document"}
					], []),
				?xml:node({"input", ?XML_NS_WSDL},
					[],[
						?xml:node({"body", ?XML_NS_SOAP12},[
								{"use", "literal"}
							],[])
					]),
				?xml:node({"output", ?XML_NS_WSDL},
					[],[
						?xml:node({"body", ?XML_NS_SOAP12},[
								{"use", "literal"}
							],[])
					])
			])
	}
];

% TODO: bindings' operations for HttpGet and HttpPost 

operation(NCN, TargetNS, http_get) -> [];

operation(_NCN, _TargetNS, _BindingType) -> [].



