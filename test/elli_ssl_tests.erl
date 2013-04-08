-module(elli_ssl_tests).
-include_lib("eunit/include/eunit.hrl").


elli_ssl_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world())
     ]}.

%%
%% TESTS
%%


hello_world() ->
    {ok, Response} = httpc:request("https://localhost:3443/hello/world"),
    ?assertEqual(200, status(Response)).

%%
%% INTERNAL HELPERS
%%


setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "test"]),
    CertFile = filename:join(CertDir, "server_cert.pem"),
    KeyFile = filename:join(CertDir, "server_key.pem"),

    {ok, P} = elli:start_link([
                               {port, 3443},
                               ssl,
                               {keyfile, KeyFile},
                               {certfile, CertFile},
                               {callback, elli_example_callback}
                               ]),
    unlink(P),
    [P].

teardown(Pids) ->
    inets:stop(),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    [elli:stop(P) || P <- Pids].

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
