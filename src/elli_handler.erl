-module(elli_handler).
-include("elli.hrl").

-callback handle(Req :: #req{}, elli:callback_args()) ->
    ignore | {elli:response_code(), [tuple()], binary()} | {ok, [tuple()], binary()}.
-callback handle_event(Event :: elli:elli_event(), Args :: [tuple()], Config :: [tuple()]) -> ok.
