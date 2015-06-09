-module(elli_http_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

%% UNIT TESTS

chunk_loop_test_() ->
    fun ()->
        Here = self(),
        ChunkLoopWrapper = fun() ->
          Result = elli_http:chunk_loop({some_type, some_socket}),
          Here ! Result,
          ok
        end,

        Pid = spawn_link(ChunkLoopWrapper),
        Pid ! {tcp_closed, some_socket},
        Message = receive
          X -> X
        after
          1 -> fail
        end,

        ?assertEqual({error, client_closed}, Message)
    end.
