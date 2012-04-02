-module(elli_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(not_found()),
      ?_test(crash()),
      ?_test(encoding()),
      ?_test(exception_flow()),
      ?_test(user_connection()),
      ?_test(get_args())
%%      ?_test(content_length())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    lhttpc:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

hello_world() ->
    URL = "http://localhost:8080/hello/world",
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "12"}], headers(Response)),
    ?assertEqual(<<"Hello World!">>, body(Response)).



not_found() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/foobarbaz", "GET", [], 1000),
    ?assertEqual({404, "Not Found"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "4"}], headers(Response)),
    ?assertEqual(<<"body">>, body(Response)).

crash() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/crash", "GET", [], 1000),
    ?assertEqual({500, "Internal Server Error"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "21"}], headers(Response)),
    ?assertEqual(<<"Internal server error">>, body(Response)).


encoding() ->
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
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), body(Response1)).


exception_flow() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/403", "GET", [], 1000),
    ?assertEqual({403, "Forbidden"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "9"}], headers(Response)),
    ?assertEqual(<<"Forbidden">>, body(Response)).

user_connection() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/close", "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "close"},
                  {"Content-Length", "7"}], headers(Response)),
    ?assertEqual(<<"closing">>, body(Response)).


get_args() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/hello?name=knut",
                                    "GET", [], 1000),
    ?assertEqual(<<"Hello knut">>, body(Response)).


%% content_length() ->
%%     S = s(),
%%     {ok, Response} = lhttpc:request("http://localhost:8080/304",
%%                                     "GET", [], 1000),

%%     ?assertEqual({304, "OK"}, status(Response)),
%%     ?assertEqual([{"Connection", "Keep-Alive"},
%%                   {"Content-Length", "0"}], headers(Response)),
%%     ?assertEqual(<<>>, body(Response)),

%%     stop(S).

%%
%% HELPERS
%%

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
