-module(elli_handover_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").


-define(i2b(I), list_to_binary(integer_to_list(I))).

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(echo())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback_handover}, {port, 3003}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


%%
%% INTEGRATION TESTS
%% Uses inets httpc to actually call Elli over the network
%%

hello_world() ->
    {ok, Response} = httpc:request("http://localhost:3003/hello/world"),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "close"},
                  {"content-length", "12"}], headers(Response)),
    ?assertEqual("Hello World!", body(Response)).

echo() ->
    {ok, Response} = httpc:request("http://localhost:3003/hello?name=knut"),
    ?assertEqual(200, status(Response)),
    ?assertEqual("Hello knut", body(Response)).



%%
%% HELPERS
%%

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
