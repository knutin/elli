-module(elli_handler).
-include("elli.hrl").

-callback handle(Req :: record(req), CallBackArgs :: callback_args()) ->
    {Code, body()} |
    {Code, headers(), RespBody} |
    {chunk, headers()} |
    {chunk, headers(), Initial}
        when Code :: ok | response_code(),
             RespBody :: {file, file:name()} | body(),
             Initial :: body().

-callback handle_event(Event :: atom(), EventInfo :: list(),
                       CallBackArgs :: callback_args()) ->
    any().
