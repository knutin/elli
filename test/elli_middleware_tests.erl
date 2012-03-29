-module(elli_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(short_circuit())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    ok = lhttpc:start(),

    Config = [
              {mods, [
                      {elli_access_log, [{name, elli_syslog},
                                         {ip, "127.0.01"},
                                         {port, 514}]},
                      {elli_example_middleware, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config}]),

    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

short_circuit() ->
    URL = "http://localhost:8080/middleware/short-circuit",
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual(<<"short circuit!">>, body(Response)).

hello_world() ->
    URL = "http://localhost:8080/hello/world",
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual(<<"Hello World!">>, body(Response)).



%%
%% HELPERS
%%

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
