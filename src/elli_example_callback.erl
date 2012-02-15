-module(elli_example_callback).
-export([handle/1, request_complete/8, request_throw/2, request_exit/2]).
-export([chunk_loop/1]).
-include("elli.hrl").

%% TODO: Turn into behaviour. Use callback return specs from R15.


handle(Req) ->
    handle(Req#req.method, elli_request:split_path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    {ok, [], <<"Hello World!">>};

handle('GET',[<<"headers.html">>], _Req) ->
    {ok, [{<<"X-Custom">>, <<"foobar">>}], <<"see headers">>};

handle('GET', [<<"crash">>], _Req) ->
    throw(foobar);

handle('GET', [<<"compressed">>], _Req) ->
    {ok, [], binary:copy(<<"Hello World!">>, 86)};

handle('GET', [<<"chunked">>], Req) ->
    Ref = elli_request:chunk_ref(Req),
    spawn(fun() -> ?MODULE:chunk_loop(Ref) end),
    {chunk, []};

handle(_, _, _Req) ->
    {404, [], <<>>}.




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




request_complete(Req, Response, AcceptStart, RequestStart,
                 HeadersEnd, BodyEnd, UserEnd, RequestEnd) ->
    %% io:format(
    %%   "REQUEST: ~s~n"
    %%   "    headers  : ~w us~n"
    %%   "    body     : ~w us~n"
    %%   "    user     : ~w us~n"
    %%   "    response : ~w us~n"
    %%   "    total    : ~w us~n",
    %%   [Req#req.path,
    %%    timer:now_diff(HeadersEnd, RequestStart),
    %%    timer:now_diff(BodyEnd, HeadersEnd),
    %%    timer:now_diff(UserEnd, BodyEnd),
    %%    timer:now_diff(RequestEnd, UserEnd),
    %%    timer:now_diff(RequestEnd, RequestStart)]),

    ok.

request_throw(Req, Exception) ->
    ok.

request_exit(Req, Exit) ->
    ok.
