%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 
-module(durden_handler_doc).
-behaviour(durden_handler).

-export([
		try_handle/3
	]).

-include("app.hrl").
-include("wsd.hrl").
-include("xml.hrl").

-define(xe(XhtmlTag, Attrs, Children), ?xml:node({XhtmlTag, ?XML_NS_XHTML}, Attrs, Children) ).
-define(id(XhtmlID), {"id", XhtmlID}).
-define(cl(Classes), {"class", Classes}).

try_handle( Module, BaseUrl, Req0 ) -> 
	{HttpMethod, ReqMethodRead} = cowboy_http_req:method(Req0),
	{QS, ReqQSRead} = cowboy_http_req:raw_qs(ReqMethodRead),
	case {HttpMethod, QS} of
		{'GET', <<>>} ->
			{ok, WSD} = durden_wsd_cache:get_wsd( Module ),

			ResponseXml = render_overall_documentation( WSD ),
			ResponseXmlWithNSs = ?xml:imp_ns(?XML_NS_XHTML, "", ResponseXml),
			{ok, ReqReplied} = cowboy_http_req:reply(200, [], ?xml:render(ResponseXmlWithNSs), ReqQSRead),
			{halt, ReqReplied};
		_ ->
			{next, ReqQSRead}
	end.

render_overall_documentation( WSD ) ->
	?xe("html", [], [
		?xe("head", [], [
			?xe("title", [], [
				WSD #wsd.service_name,
				" - Web-Service Documentation"
			]),
			?xe("style", [], [
				css_style()
			])
		]),
		?xe("body", [], [
			?xe("h3", [], [ WSD #wsd.service_name ]),
			?xe("h4", [], [ 
				"Target namespace: ",
				WSD #wsd.target_ns
			]),

			?xe("ul", [?id("service-actions")], [
				?xe("li", 
					[
						?id("service-action-" ++ FuncName),
						?cl("service-action")
					], render_service_action(FuncName, WSD))
				|| FuncName <- 
					lists:sort( ?dict_m:fetch_keys( 
						?dict_m:fetch( 
							durden_wsd_aux:resolve_ns(WSD #wsd.target_ns, tns_funcs),
							WSD #wsd.schemas
						)
					) )
			])
		])
	]).

render_service_action( ActionName, WSD ) ->
	[
		?xe("div", [?cl("action-name")], [ActionName]),
		?xe("div", [?cl("action-input")], render_service_action_input( ActionName, WSD )),
		?xe("div", [?cl("action-output")], render_service_action_output( ActionName, WSD ))
	].

render_service_action_input( _ActionName, _WSD ) -> 
	[ 
		"Here the input will be described..." 
	].
render_service_action_output( _ActionName, _WSD ) -> 
	[
		"Here the output will be described..."
	].


css_style() ->
	"
body {
	background: #eee;
}
li.service-action {
	background: #999;
	font-size: 15pt;
}
".
