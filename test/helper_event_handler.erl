-module(helper_event_handler).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).

handle(_Req, _Args) -> ignore.

handle_event(request_throw, [_Request, _Exception, _Stacktrace], _) ->
    helper_events_server:add(request_throw),
    ok;
handle_event(_, _, _) -> ok.