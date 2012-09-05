# Durden - a tool to make SOAP with Erlang and Cowboy web-server.

## Docs

* http://www.w3.org/TR/2000/NOTE-SOAP-20000508/

* http://www.w3.org/TR/wsdl

## Current status

Durden can generate a not too bad WSDL from specially cooked erlang modules.
It also can handle SOAP/1.2-bound requests.

## Nearest future

Soap transports:
* HTTP-POST
* SOAP/1.1
* HTTP-GET

Support of indirect reference loops in the types declarations (currently parse transformation hangs when analysing a types like "A references B references A again")

Human readable documentation page (alike to that one generated when you visit .Net's ASMX page with no parameters)

## How it wokrs

Durden analyses the source of the module supposed to become a Web-service via parse-transform.

The specs of the functions enlisted in 'soap_actions' attribute are analysed.

All the mentioned types are gathered recursively.

The type info is then used as the return value of a generated function Module:'#durden.get_wsd#'/0.

Besides the latter, 'cowboy_http_handler' behaviour is implemented: init/3 callback just asks Cowboy to upgrade the protocol into 'durden_cowboy_http_soap'.

## Example

(https://github.com/RGafiyatullin/durden_examples)

The following module:
<pre>
-module(soap_test_service).
-compile({parse_transform, durden_pt}).

-include_lib("durden/include/predefined_types.hrl").

-soap_target_ns("http://rgafiyatullin.github.com/test-service").
-soap_service_name("TestService").

-soap_actions(['GetUserByID'/1]).

-type phone_numbers() :: [ string() ].

-record('User', {
	'ID' :: uuid(),
	'Name' :: string(),
	'YearOfBirth' :: integer(),
	'Phones' :: phone_numbers()
	}).

-spec 'GetUserByID'( ID :: uuid() ) -> #'User'{}.
'GetUserByID'( ID ) ->
	#'User'{
		'ID' = ID,
		'Name' = "Test User",
		'YearOfBirth' = 1988,
		'Phones' = ["+375005550000", "+375115550000"]
	}.
</pre>

produces WSDL below:
(alternatively can be viewed at https://gist.github.com/3364435)

<pre>

<?xml version="1.0"?>
<wsdl:definitions xmlns:ms="http://microsoft.com/wsdl/types/" xmlns:mstm="http://microsoft.com/wsdl/mime/textMatching/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:tns="http://rgafiyatullin.github.com/test-service" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:http="http://schemas.xmlsoap.org/wsdl/http/" xmlns:s0="http://rgafiyatullin.github.com/test-service/s0" xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:mime="http://schemas.xmlsoap.org/wsdl/mime/" targetNamespace="http://rgafiyatullin.github.com/test-service">
  <wsdl:types>
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://rgafiyatullin.github.com/test-service/s0">
      <xs:element name="GetUserByIDResponse">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="GetUserByIDResult" type="s0:User" minOccurs="1" maxOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="GetUserByID">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ID" type="xs:guid" minOccurs="1" maxOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:complexType name="User">
        <xs:sequence>
          <xs:element name="ID" type="xs:guid" minOccurs="1" maxOccurs="1"/>
          <xs:element name="Name" type="xs:string" minOccurs="1" maxOccurs="1"/>
          <xs:element name="YearOfBirth" type="xs:int" minOccurs="1" maxOccurs="1"/>
          <xs:element name="Phones" type="s0:phone_numbers" minOccurs="1" maxOccurs="1"/>
        </xs:sequence>
      </xs:complexType>
      <xs:complexType name="phone_numbers">
        <xs:sequence>
          <xs:element minOccurs="0" maxOccurs="unbounded" name="string" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>
  </wsdl:types>
  <wsdl:message name="GetUserByIDSoapIn">
    <wsdl:part name="parameters" element="s0:GetUserByID"/>
  </wsdl:message>
  <wsdl:message name="GetUserByIDSoapOut">
    <wsdl:part name="parameters" element="s0:GetUserByIDResponse"/>
  </wsdl:message>
  <wsdl:portType name="TestServiceSoap">
    <wsdl:operation name="GetUserByID">
      <wsdl:input message="tns:GetUserByIDSoapIn"/>
      <wsdl:output message="tns:GetUserByIDSoapOut"/>
    </wsdl:operation>
  </wsdl:portType>
  <wsdl:portType name="TestServiceSoap12">
    <wsdl:operation name="GetUserByID">
      <wsdl:input message="tns:GetUserByIDSoapIn"/>
      <wsdl:output message="tns:GetUserByIDSoapOut"/>
    </wsdl:operation>
  </wsdl:portType>
  <wsdl:binding name="TestServiceSoap" type="tns:TestServiceSoap">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http"/>
    <wsdl:operation name="GetUserByID">
      <soap:operation soapAction="http://rgafiyatullin.github.com/test-service/GetUserByID" style="document"/>
      <wsdl:input>
        <soap:body use="literal"/>
      </wsdl:input>
      <wsdl:output>
        <soap:body use="literal"/>
      </wsdl:output>
    </wsdl:operation>
  </wsdl:binding>
  <wsdl:binding name="TestServiceSoap12" type="tns:TestServiceSoap12">
    <soap12:binding transport="http://schemas.xmlsoap.org/soap/http"/>
    <wsdl:operation name="GetUserByID">
      <soap12:operation soapAction="http://rgafiyatullin.github.com/test-service/GetUserByID" style="document"/>
      <wsdl:input>
        <soap12:body use="literal"/>
      </wsdl:input>
      <wsdl:output>
        <soap12:body use="literal"/>
      </wsdl:output>
    </wsdl:operation>
  </wsdl:binding>
  <wsdl:service name="TestService">
    <wsdl:port name="TestServiceSoap" binding="tns:TestServiceSoap">
      <soap:address location="http://localhost:8080/test/service.asmx"/>
    </wsdl:port>
    <wsdl:port name="TestServiceSoap12" binding="tns:TestServiceSoap12">
      <soap12:address location="http://localhost:8080/test/service.asmx"/>
    </wsdl:port>
  </wsdl:service>
</wsdl:definitions>

</pre>


