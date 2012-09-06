%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(durden_xml_hrl).
-define(durden_xml_hrl, included).

-type xml_ns() :: string().
-type xml_ncname() :: string().
-type xml_qname() :: { xml_ns(), xml_ncname() }.

-define(xml, durden_xml_aux).

-define(XML_NS_WSDL, "http://schemas.xmlsoap.org/wsdl/").
-define(XML_NS_SOAP, "http://schemas.xmlsoap.org/wsdl/soap/").
-define(XML_NS_SOAP12, "http://schemas.xmlsoap.org/wsdl/soap12/").
-define(XML_NS_SOAPENV, "http://schemas.xmlsoap.org/soap/envelope/").
-define(XML_NS_SOAPENC, "http://schemas.xmlsoap.org/soap/encoding/").
-define(XML_NS_HTTP, "http://schemas.xmlsoap.org/wsdl/http/").
-define(XML_NS_MIME, "http://schemas.xmlsoap.org/wsdl/mime/").
-define(XML_NS_MS_TM, "http://microsoft.com/wsdl/mime/textMatching/").
-define(XML_NS_XSD, "http://www.w3.org/2001/XMLSchema").
-define(XML_NS_XSI, "http://www.w3.org/2001/XMLSchema-instance").
-define(XML_NS_MS, "http://microsoft.com/wsdl/types/").

-define(XML_NS_SOAP_TRANSPORT, "http://schemas.xmlsoap.org/soap/http").

-endif. % durden_xml_hrl
