-module(elli_tests).
-include_lib("eunit/include/eunit.hrl").


elli_test_() ->
    {setup,
     fun start_deps/0, fun stop_deps/1,
     [
      ?_test(hello_world()),
      ?_test(not_found()),
      ?_test(crash()),
      ?_test(encoding())
     ]}.



start_deps() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    ok = lhttpc:start().

stop_deps(_) ->
    ok = lhttpc:stop().

s() ->
    {ok, P} = elli:start_link([{callback, elli_example_callback}]),
    unlink(P),
    P.

hello_world() ->
    S = s(),
    URL = "http://localhost:8080/hello/world",
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "12"}], headers(Response)),
    ?assertEqual(<<"Hello World!">>, body(Response)),
    stop(S).


not_found() ->
    S = s(),
    {ok, Response} = lhttpc:request("http://localhost:8080/foobarbaz", "GET", [], 1000),
    ?assertEqual({404, "Not Found"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "0"}], headers(Response)),
    ?assertEqual(<<>>, body(Response)),
    stop(S).

crash() ->
    S = s(),
    {ok, Response} = lhttpc:request("http://localhost:8080/crash", "GET", [], 1000),
    ?assertEqual({500, "Internal Server Error"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "0"}], headers(Response)),
    ?assertEqual(<<>>, body(Response)),
    stop(S).

encoding() ->
    S = s(),
    {ok, Response} = lhttpc:request("http://localhost:8080/compressed", "GET",
                                    [{"Accept-Encoding", "gzip"}], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Encoding", "gzip"},
                  {"Content-Length", "41"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), zlib:gunzip(body(Response))),

    {ok, Response1} = lhttpc:request("http://localhost:8080/compressed", "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response1)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "1032"}], headers(Response1)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), body(Response1)),
    stop(S).


%%
%% HELPERS
%%

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).


stop(S) ->
    ok = elli:stop(S).
