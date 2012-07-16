-module(elli_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(short_circuit()),
      ?_test(compress())
     ]}.

%%
%% TESTS
%%


short_circuit() ->
    URL = "http://localhost:8080/middleware/short-circuit",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("short circuit!", body(Response)).

hello_world() ->
    URL = "http://localhost:8080/hello/world",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("Hello World!", body(Response)).


compress() ->
    {ok, Response} = httpc:request(get, {"http://localhost:8080/compressed",
                                         [{"Accept-Encoding", "gzip"}]}, [], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-encoding", "gzip"},
                  {"content-length", "41"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), zlib:gunzip(body(Response))),

    {ok, Response1} = httpc:request("http://localhost:8080/compressed"),
    ?assertEqual(200, status(Response1)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response1)),
    ?assertEqual(lists:flatten(lists:duplicate(86, "Hello World!")), body(Response1)).



%%
%% HELPERS
%%

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).


setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    %% elli_example_callback:module_info(),
    %% elli_middleware_compress:module_info(),
    %% elli_example_middleware:module_info(),
    %% elli_access_log:module_info(),

    Config = [
              {mods, [
                      {elli_access_log, [{name, elli_syslog},
                                         {ip, "127.0.01"},
                                         {port, 514}]},
                      {elli_example_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config}]),
    unlink(P),
    register(elli, P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

