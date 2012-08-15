%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsdl_portops).
-compile({parse_transform, gin}).
-export([operations/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsdl.hrl").
-include("erl_types.hrl").

port_types() ->
	[ soap, soap12, http_get, http_post ].

-spec operations( xml_ncname(), xml_ns(), erlang_type_def() ) -> [{atom(), ?xml:xml_node()}].
operations( NCN, TargetNS, #et_func{} ) ->
	operations(NCN, TargetNS);
operations( _, _, _ ) -> [].

operations( NCN, TargetNS ) ->
	lists:flatten([
		operation(NCN, TargetNS, PortType)
		|| PortType <- port_types()
	]).

operation( NCN, TargetNS, AnySoap ) when in(AnySoap, [ soap, soap12 ]) ->
	[
		{
			AnySoap,
			?xml:node({"operation", ?XML_NS_WSDL},
				[{"name", NCN}],
				[
					?xml:node({"input", ?XML_NS_WSDL},
						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_SOAP_IN, TargetNS) } ],
						[]
					),
					?xml:node({"output", ?XML_NS_WSDL}, 
						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_SOAP_OUT, TargetNS) } ],
						[]
					)
				])
		}
	];

%% TODO: portTypes' oeprations for HttpGet and HttpPost
% operation( NCN, TargetNS, http_get ) ->
% 	[
% 		{
% 			http_get,
% 			?xml:node({"operation", ?XML_NS_WSDL},
% 				[{"name", NCN}],
% 				[
% 					?xml:node({"input", ?XML_NS_WSDL},
% 						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_HTTP_GET_IN, TargetNS) } ],
% 						[]
% 					),
% 					?xml:node({"output", ?XML_NS_WSDL}, 
% 						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_HTTP_GET_OUT, TargetNS) } ],
% 						[]
% 					)
% 				])
% 		}
% 	];

% operation( NCN, TargetNS, http_get ) ->
% 	[
% 		{
% 			http_post,
% 			?xml:node({"operation", ?XML_NS_WSDL},
% 				[{"name", NCN}],
% 				[
% 					?xml:node({"input", ?XML_NS_WSDL},
% 						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_HTTP_POST_IN, TargetNS) } ],
% 						[]
% 					),
% 					?xml:node({"output", ?XML_NS_WSDL}, 
% 						[ {"message", ?xml:qname(NCN ++ ?WSDL_SUFFIX_HTTP_POST_OUT, TargetNS) } ],
% 						[]
% 					)
% 				])
% 		}
% 	];

operation( _NCN, _TargetNS, _ ) -> [].

