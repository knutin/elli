-module(elli_example_callback).
-export([handle/2, request_complete/9]).
-export([request_throw/4, request_exit/4, request_error/4]).
-export([chunk_loop/1]).
-include("elli.hrl").
-behaviour(elli_handler).

%% TODO: Turn into behaviour. Use callback return specs from R15.


handle(Req, _Args) ->
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

handle('GET', [<<"304">>], _Req) ->
    {304, [], <<>>};

handle(_, _, _Req) ->
    {404, [], <<"body">>}.




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




request_complete(_Req, _Response, _AcceptStart, _RequestStart,
                 _HeadersEnd, _BodyEnd, _UserEnd, _RequestEnd, _Args) ->
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

request_throw(_Req, _Exception, _Stack, _Args) ->
    ok.

request_exit(_Req, _Exit, _Stack, _Args) ->
    ok.

request_error(_Req, _Error, _Stack, _Args) ->
    ok.
