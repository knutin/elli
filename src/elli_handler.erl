-module(elli_handler).
-include("elli.hrl").

-callback handle(Request :: #req{}, callback_args()) ->
    {ok, headers(), body()} |
    {response_code(), headers(), body()} |
    {chunk, headers()} |
    no_return().

-callback request_complete(#req{}, response(),
                           AcceptStart :: timestamp(), RequestStart :: timestamp(),
                           HeadersEnd :: timestamp(), BodyEnd :: timestamp(),
                           UserEnd :: timestamp(), RequestEnd :: timestamp(),
                          callback_args()) ->
    ok.

-callback request_throw(#req{}, term(), callback_args()) -> ok.
-callback request_exit(#req{}, term(), callback_args()) -> ok.
-callback request_error(#req{}, term(), callback_args()) -> ok.
