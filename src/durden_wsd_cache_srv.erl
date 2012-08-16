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

handle_call({get_cache_entry, Handler}, _From, State = #s{ ets = ETS }) ->
	% TODO: URI shoult be determined not hardcoded.
	{Wsdl, Wsd} = case ets:lookup(ETS, Handler) of
		[] ->
			io:format("Cache[~p] MISS~n", [Handler]),
			{ok, WsdlCreated, WsdCreated} = durden_wsdl:module_wsdl("http://localhost:8080/test/service.asmx", Handler),
			true = ets:insert_new(ETS, { Handler, {WsdlCreated, WsdCreated} }),
			{WsdlCreated, WsdCreated};
		[ { _, {WsdlFound, WsdFound} } ] ->
			io:format("Cache[~p] HIT~n", [Handler]),
			{WsdlFound, WsdFound}
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
