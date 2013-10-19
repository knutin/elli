-module(elli_upgrade_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").
-include("elli_util.hrl").


-define(i2b(I), list_to_binary(integer_to_list(I))).
disallowed_upgrade_test_() ->
    {setup,
     fun setup_without_upgrade/0, fun teardown/1,
     [
      	?_test(upgrade_when_not_allowed())
     ]}.

allowed_upgrade_test_() ->
    {setup,
     fun setup_with_upgrade/0, fun teardown/1,
     [
      	?_test(upgrade_allowed())
     ]}.


setup_without_upgrade() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback}, {port, 3001}]),
    unlink(P),
    [P].

setup_with_upgrade() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback}, {port, 3001}, {upgrade, true}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


%%
%% Upgrade tests. 1 when enabled, 1 when not
%%

upgrade_when_not_allowed() ->
    Response = httpc:request(get, {"http://localhost:3001/websocket",
        [{"Upgrade", "websocket"}]}, [], []),
    ?assertEqual({error, socket_closed_remotely}, Response).

upgrade_allowed() ->
    Response = httpc:request(get, {"http://localhost:3001/websocket",
        [{"Upgrade", "websocket"}]}, [], []),
    ?assertEqual({error, socket_closed_remotely}, Response).