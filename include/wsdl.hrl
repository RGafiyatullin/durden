%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(durden_wsdl_hrl).
-define(durden_wsdl_hrl, included).

-define(WSDL_FIELD_RESULT(ReqName), list_to_atom(ReqName ++ "Result") ).
-define(WSDL_SUFFIX_RESPONSE, "Response").

-define(WSDL_SUFFIX_PT_SOAP, "Soap").
-define(WSDL_SUFFIX_PT_SOAP12, "Soap12").
-define(WSDL_SUFFIX_PT_HTTP_GET, "HttpGet").
-define(WSDL_SUFFIX_PT_HTTP_POST, "HttpPost").

-define(WSDL_SUFFIX_SOAP_IN, "SoapIn").
-define(WSDL_SUFFIX_SOAP_OUT, "SoapOut").
-define(WSDL_SUFFIX_HTTP_GET_IN, "HttpGetIn").
-define(WSDL_SUFFIX_HTTP_GET_OUT, "HttpGetOut").
-define(WSDL_SUFFIX_HTTP_POST_IN, "HttpPostIn").
-define(WSDL_SUFFIX_HTTP_POST_OUT, "HttpPostOut").


-endif. % durden_wsdl_hrl
