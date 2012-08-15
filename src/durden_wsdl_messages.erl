%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsdl_messages).
-export([messages/2]).
-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("erl_types.hrl").
-include("wsdl.hrl").

message_types() -> [soap, get, post]. 

-spec messages(
		{xml_ncname(), xml_ns()},
		erlang_type_def()
	) -> [ { xml_ncname(), ?xml:xml_node() } ].
messages(
	{NCN, NS},
	#et_func{
		args = Args,
		ret = Ret
	}
) -> 
	lists:flatten([
		[
			message_in( NCN, NS, Args, MsgType ),
			message_out( NCN, NS, Ret, MsgType )
		] ||
		MsgType <- message_types()
	]);
messages( {_NCN, _NS}, _TypeDef ) -> [].

message_in( NCN, NS, _Args, soap ) -> [
	?xml:node({"message", ?XML_NS_WSDL},
		[ {"name", NCN ++ ?WSDL_SUFFIX_SOAP_IN} ],
		[
			?xml:node({"part", ?XML_NS_WSDL},
				[ 
					{"name", "parameters"},
					{"element", ?xml:qname(NCN, NS) }
				],
				[])
		])
];
message_in( NCN, _NS, _Args, get ) -> [
	% ?xml:node({"message", ?XML_NS_WSDL},
	% 	[ {"name", NCN ++ ?WSDL_SUFFIX_HTTP_GET_IN} ],
	% 	[])
];
message_in( NCN, _NS, _Args, post ) -> [
	% ?xml:node({"message", ?XML_NS_WSDL},
	% 	[ {"name", NCN ++ ?WSDL_SUFFIX_HTTP_POST_IN} ],
	% 	[])
].

message_out( NCN, NS, _Ret, soap ) -> [
	?xml:node({"message", ?XML_NS_WSDL},
		[ {"name", NCN ++ ?WSDL_SUFFIX_SOAP_OUT} ],
		[
			?xml:node({"part", ?XML_NS_WSDL},
				[
					{"name", "parameters"},
					{"element", ?xml:qname(NCN ++ ?WSDL_SUFFIX_RESPONSE, NS)}
				],
				[])
		])
];
message_out( NCN, _NS, _Ret, get ) -> [
	% ?xml:node({"message", ?XML_NS_WSDL},
	% 	[ {"name", NCN ++ ?WSDL_SUFFIX_HTTP_GET_OUT} ],
	% 	[])
];
message_out( NCN, _NS, _Ret, post ) -> [
	% ?xml:node({"message", ?XML_NS_WSDL},
	% 	[ {"name", NCN ++ ?WSDL_SUFFIX_HTTP_POST_OUT} ],
	% 	[])
].

