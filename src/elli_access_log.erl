%% @doc HTTP access and error log, sending to syslog over UDP
%%
%% Sends a simple log line for every request, even errors, to
%% syslog. The line includes the following timings, all specified in
%% wall-clock microseconds:
%%
%% RequestLine/Headers/Body/User/Response/Total
%%
%% * RequestLine: time between accept returning and complete receive
%%   of the request line, ie. "GET /foo HTTP/1.1". If keep-alive is
%%   used, this will be the time since the initial accept so it might
%%   be very high.
%%
%% * Headers: Time to receive all headers
%%
%% * Body: Time to receive the entire body into memory, not including
%%   any decoding
%%
%% * User: Time spent in the callback. If middleware is used, the
%%   runtime of the middleware is included in this number
%%
%% * Response: Time taken to send the response to the client
%%
%% * Total: The time between the request line was received and the
%%   response was sent. This is as close we can get to the actual time
%%   of the request as seen by the user.

-module(elli_access_log).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).


handle(_Req, _Args) ->
    %% We are installed as a middleware, ignore everything.
    ignore.


handle_event(request_complete, [Req, ResponseCode, _ResponseHeaders,
                                ResponseBody, Timings], Args) ->

    %% The Elli request process is done handling the request, so we
    %% can afford to do some heavy lifting here.

    Accepted     = proplists:get_value(accepted, Timings),
    RequestStart = proplists:get_value(request_start, Timings),
    HeadersEnd   = proplists:get_value(headers_end, Timings),
    BodyEnd      = proplists:get_value(body_end, Timings),
    UserStart    = proplists:get_value(user_start, Timings),
    UserEnd      = proplists:get_value(user_end, Timings),
    RequestEnd   = proplists:get_value(request_end, Timings),

    TimeStr = io_lib:format("~w/~w/~w/~w/~w/~w",
                            [timer:now_diff(RequestStart, Accepted),
                             timer:now_diff(HeadersEnd, RequestStart),
                             timer:now_diff(BodyEnd, HeadersEnd),
                             timer:now_diff(UserEnd, UserStart),
                             timer:now_diff(RequestEnd, UserEnd),
                             timer:now_diff(RequestEnd, RequestStart)]),

    Msg = io_lib:format("~s ~s ~w ~w \"~s ~s\"",
                        [elli_request:peer(Req),
                         TimeStr,
                         ResponseCode,
                         iolist_size(ResponseBody),
                         elli_request:method(Req),
                         elli_request:raw_path(Req)
                        ]),

    elli_access_log_server:log(name(Args), Msg),
    ok;

handle_event(request_throw, [Req, Exception, Stack], _Args) ->
    error_logger:error_msg("exception: ~p~nstack: ~p~nrequest: ~p~n",
                           [Exception, Stack, elli_request:to_proplist(Req)]),
    ok;
handle_event(request_exit, [Req, Exit, Stack], _Args) ->
    error_logger:error_msg("exit: ~p~nstack: ~p~nrequest: ~p~n",
                           [Exit, Stack, elli_request:to_proplist(Req)]),
    ok;

handle_event(request_error, [Req, Error, Stack], _Args) ->
    error_logger:error_msg("error: ~p~nstack: ~p~nrequest: ~p~n",
                           [Error, Stack, elli_request:to_proplist(Req)]),
    ok;

handle_event(request_parse_error, [_Data], _Args) ->
    ok;
handle_event(bad_request, _Data, _Args) ->
    ok;

handle_event(client_closed, [_When], _Args) ->
    ok;
handle_event(client_timeout, [_When], _Args) ->
    ok;

handle_event(elli_startup, [], Args) ->
    {ok, _} = elli_access_log_server:start_link(name(Args), facility(Args)),

    case whereis(name(Args)) of
        undefined ->
            {ok, _Pid} = syslog:start_link(name(Args),
                                           proplists:get_value(ip, Args),
                                           proplists:get_value(port, Args)),
            ok;
        Pid when is_pid(Pid) ->
            ok
    end.


name(Config) -> proplists:get_value(name, Config).
facility(Config) -> proplists:get_value(facility, Config, local0).

