-module(durden_transport).

-callback try_handle( Handler :: atom(), Req :: term() ) -> 
	{reject, Req :: term()} |
	{accept, Req :: term()}.
