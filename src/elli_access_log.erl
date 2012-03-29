-module(elli_access_log).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).


handle(_Req, _Args) ->
    ignore.

handle_event(request_complete, [Req, ResponseCode, _ResponseHeaders,
                                ResponseBody, _Timings], Config) ->

    Msg = io_lib:format("~s \"~s ~s\" ~w ~w",
                        [elli_request:peer(Req),
                         elli_request:method(Req),
                         elli_request:raw_path(Req),
                         ResponseCode,
                         byte_size(ResponseBody)]),

    syslog:send(proplists:get_value(name, Config), Msg, [{ident, node()}]),
    ok;

handle_event(request_throw, [_Req, Exception, Stack], _Config) ->
    error_logger:error_msg("exception: ~p~n stack: ~p~n", [Exception, Stack]),
    ok;
handle_event(request_exit, [_Req, Exit, Stack], _Config) ->
    error_logger:error_msg("exit: ~p~n stack: ~p~n", [Exit, Stack]),
    ok;

handle_event(request_error, [_Req, Error, Stack], _Config) ->
    error_logger:error_msg("error: ~p~n stack: ~p~n", [Error, Stack]),
    ok;

handle_event(request_parse_error, [_Data], _Args) ->
    ok;

handle_event(client_closed, [_When], _Config) ->
    ok;

handle_event(client_timeout, [_When], _Config) ->
    ok;
handle_event(elli_startup, [], Config) ->
    case whereis(proplists:get_value(name, Config)) of
        undefined ->
            {ok, _Pid} = syslog:start_link(proplists:get_value(name, Config),
                                          proplists:get_value(ip, Config),
                                          proplists:get_value(port, Config)),
            ok;
        Pid when is_pid(Pid) ->
            ok
    end.




