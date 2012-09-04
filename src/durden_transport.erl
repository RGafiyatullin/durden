-module(durden_transport).

% -callback try_handle( Handler :: atom(), Req :: term() ) -> 
% 	{reject, Req :: term()} |
% 	{accept, Req :: term()}.

-callback can_handle( 
	Handler :: atom(), 
	Req :: term()
) -> 
	{boolean(), ReqUsed :: term() }.
-callback parse_request( 
	Handler :: atom(), 
	Req :: term() 
) -> 
	{ ok, F :: atom(), A :: [ term() ], ReqUsed :: term() }.
-callback render_response(
	Handler :: atom(),
	RetValue :: term(),
	Req :: term()
) -> 
	{ ok, ReqResponded :: term() }.

-callback render_error(
	Error :: term(),
	Req :: term()
	) ->
	{ok, ReqResponded :: term()}.
