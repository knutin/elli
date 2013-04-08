-module(elli_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

-define(TRACE_TABLE, middleware_tests).


elli_middleware_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(short_circuit()),
      ?_test(timings()),
      ?_test(compress())
     ]}.

%%
%% TESTS
%%


short_circuit() ->
    URL = "http://localhost:3002/middleware/short-circuit",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("short circuit!", body(Response)).

hello_world() ->
    URL = "http://localhost:3002/hello/world",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("Hello World!", body(Response)).

timings() ->
    elli_middleware_trace:clear(?TRACE_TABLE),

    {ok, _} = httpc:request(get, {"http://localhost:3002/compressed",
                                  [{"Accept-Encoding", "gzip"}]}, [], []),

    [{_, request_complete, [_Request, _ResponseCode, _ResponseHeaders,
                            _ResponseBody, Timings]}] = ets:tab2list(?TRACE_TABLE),
    ?assert(proplists:is_defined(
              {middleware_postprocess_end, elli_middleware_compress}, Timings)),
    ok.


compress() ->
    {ok, Response} = httpc:request(get, {"http://localhost:3002/compressed",
                                         [{"Accept-Encoding", "gzip"}]}, [], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-encoding", "gzip"},
                  {"content-length", "41"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), zlib:gunzip(body(Response))),

    {ok, Response1} = httpc:request("http://localhost:3002/compressed"),
    ?assertEqual(200, status(Response1)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response1)),
    ?assertEqual(lists:flatten(lists:duplicate(86, "Hello World!")), body(Response1)),

    {ok, Response2} = httpc:request(get, {"http://localhost:3002/compressed-io_list",
                                         [{"Accept-Encoding", "gzip"}]}, [], []),
    ?assertEqual(200, status(Response2)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-encoding", "gzip"},
                  {"content-length", "41"}], headers(Response2)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86), zlib:gunzip(body(Response))),

    {ok, Response3} = httpc:request("http://localhost:3002/compressed-io_list"),
    ?assertEqual(200, status(Response3)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response3)),
    ?assertEqual(lists:flatten(lists:duplicate(86, "Hello World!")), body(Response3)).





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

    Config = [
              {mods, [
                      {elli_access_log, [{name, elli_syslog},
                                         {ip, "127.0.0.1"},
                                         {port, 514}]},
                      {elli_example_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_middleware_trace, [{name, ?TRACE_TABLE}]},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3002}]),
    unlink(P),
    [P].

teardown(Pids) ->
    application:stop(crypto),
    application:stop(public_key),
    application:stop(ssl),
    inets:stop(),
    [elli:stop(P) || P <- Pids].


