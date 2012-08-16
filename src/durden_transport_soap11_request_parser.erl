-module(durden_transport_soap11_request_parser).
-export([get_request_args/3]).

-include("app.hrl").
-include("xml.hrl").
-include("wsd.hrl").
-include("erl_types.hrl").

-define('SOAP:Envelope', "{http://schemas.xmlsoap.org/soap/envelope/}Envelope").
-define('SOAP:Body', "{http://schemas.xmlsoap.org/soap/envelope/}Body").

-spec get_request_args( 
	ReqXml :: term(), 
	{FuncName :: string(), FuncsNS :: xml_ns()}, 
	WSD :: #wsd{}
) -> [any()].

get_request_args( ReqXml, Func = {_FuncName, _FuncsNS}, WSD ) ->
	{ok, SoapBody} = extract_soap_body( ReqXml ),
	{ok, Message} = extract_soap_message( SoapBody ),
	{ok, Args} = extract_func_args( Message, Func, WSD ).


extract_soap_body( {?'SOAP:Envelope', _, [ SoapBody ]} ) -> {ok, SoapBody}.
extract_soap_message( {?'SOAP:Body', _, [ Message ]} ) -> {ok, Message}.

extract_func_args(
	Message = {MsgType, _MsgAttrs, _MsgChildren},
	{_FuncName, _FuncsNS},
	_WSD = #wsd{ schemas = _Schemas }
) ->
	io:format("Message: ~p~n", [MsgType]),
	{ok, []}.
