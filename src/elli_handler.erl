-module(elli_handler).
-include("elli.hrl").

-callback handle(Request ::#req{}) ->
    {ok, headers(), body()} |
    {response_code(), headers(), body()}.
