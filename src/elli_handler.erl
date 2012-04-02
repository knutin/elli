-module(elli_handler).
-include("elli.hrl").

-callback handle(Request :: #req{}, callback_args()) ->
    {ok, headers(), body()} |
    {response_code(), headers(), body()} |
    {chunk, headers()} |
    no_return().

-type request_event() :: request_complete
                       | request_throw
                       | request_exit
                       | request_error
                       | request_parse_error
                       | client_closed
                       | client_timeout.

-callback handle_event(request_event(), list(), callback_args()) -> ok.
