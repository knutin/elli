-module(elli_handler).
-include("elli.hrl").

-callback handle(Request :: #req{}, callback_args()) ->
    {ok, headers(), body()} |
    {response_code(), headers(), body()} |
    {chunk, headers()} |
    no_return().

-callback request_complete(#req{}, response_code(), headers(), body(),
                           [{atom(), term()}], callback_args()) ->
    ok.

-callback request_throw(#req{}, term(), term(), callback_args()) -> ok.
-callback request_exit(#req{}, term(), term(), callback_args()) -> ok.
-callback request_error(#req{}, term(), term(), callback_args()) -> ok.
-callback request_parse_error(binary(), callback_args()) -> ok.
-callback client_closed(term(), callback_args()) -> ok.
-callback client_timeout(term(), callback_args()) -> ok.

