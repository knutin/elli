-module(elli_example).
-export([handle/1, chunk_loop/1]).

%% TODO: Turn into behaviour. Use callback return specs from R15.


handle(Req) ->
    case elli_request:split_path(Req) of
        [<<"hello">>, <<"world">>] ->
            {ok, [], <<"Hello World!">>};
        [<<"headers.html">>] ->
            {ok, [{<<"X-Custom">>, <<"foobar">>}], <<"see headers">>};
        [<<"chunked">>] ->
            Ref = elli_request:chunk_ref(Req),
            spawn(fun() -> ?MODULE:chunk_loop(Ref) end),
            {chunk, []};
        _ ->
            {404, [], <<>>}
    end.

chunk_loop(Ref) ->
    chunk_loop(Ref, 10).

chunk_loop(Ref, 0) ->
    elli_request:send_chunk(Ref, <<>>);
chunk_loop(Ref, N) ->
    timer:sleep(1000),

    case elli_request:send_chunk(Ref, [<<"chunk">>, integer_to_list(N)]) of
        ok -> ok;
        {error, Reason} ->
            io:format("error in sending chunk: ~p~n", [Reason])
    end,

    chunk_loop(Ref, N-1).
