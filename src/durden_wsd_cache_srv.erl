%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-module(durden_wsd_cache_srv).
-behaviour(gen_server).
-export([
	start_link/0
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(s, {
	ets :: atom() | ets:tid()
	}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	?MODULE = ets:new(?MODULE, [named_table, set, protected]),
	{ok, #s{
		ets = ?MODULE
	}}.

handle_call({get_cache_entry, Handler, BaseUrl}, _From, State = #s{ ets = ETS }) ->
	WSDKey = {wsd, Handler},
	WSDLKey = {wsdl, Handler, BaseUrl},
	{ok, Wsd} = case ets:lookup(ETS, WSDKey) of
		[] ->
			{ok, _WsdlCreated, WsdCreated} = durden_wsdl:module_wsdl("http://example.com/", Handler),
			true = ets:insert_new(ETS, { WSDKey, WsdCreated }),
			{ok, WsdCreated};
		[ {_, WsdFound} ] ->
			{ok, WsdFound}
	end,
	{ok, Wsdl} = case ets:lookup(ETS, WSDLKey) of
		[] ->
			{ok, WsdlCreated, _} = durden_wsdl:module_wsdl(BaseUrl, Handler),
			true = ets:insert_new(ETS, { WSDLKey, WsdlCreated }),
			{ok, WsdlCreated};
		[ {_, WsdlFound} ] ->
			{ok, WsdlFound}
	end,
	{reply, {ok, Wsdl, Wsd}, State};

handle_call(Request, _From, State = #s{}) ->
	{stop, {bad_arg, Request}, State}.

handle_cast(Request, State = #s{}) ->
	{stop, {bad_arg, Request}, State}.

handle_info(Message, State = #s{}) ->
	{stop, {bad_arg, Message}, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
