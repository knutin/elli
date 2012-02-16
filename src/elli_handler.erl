-module(elli_handler).
-include("elli.hrl").

-callback handle(Request :: #req{}) ->
    {ok, headers(), body()} |
    {response_code(), headers(), body()} |
    {chunk, headers()} |
    no_return().

-callback request_complete(#req{}, response(),
                           AcceptStart :: timestamp(), RequestStart :: timestamp(),
                           HeadersEnd :: timestamp(), BodyEnd :: timestamp(),
                           UserEnd :: timestamp(), RequestEnd :: timestamp()) ->
    ok.

-callback request_throw(#req{}, term()) -> ok.
-callback request_exit(#req{}, term()) -> ok.
-callback request_error(#req{}, term()) -> ok.
