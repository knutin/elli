-module(elli).
-compile([export_all]).

-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [binary, {reuseaddr, true}, {packet, 0}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    socket_loop(Socket),
    exit(normal).

socket_loop(Socket) ->
    %% keep alive
    inet:setopts(Socket, [{packet, http}]),
    request_loop(Socket).

request_loop(Socket) ->
    {Method, Path, Headers} = get_headers(Socket),
    Body = get_body(Socket, Headers),

    {ok, UserHeaders, UserBody} = user_callback(Method, Path, Headers, Body),

    ResultHeaders = [{'Content-Length', size(UserBody)} | UserHeaders],

    Response = [<<"HTTP/1.1 200 OK\r\n">>,
                encode_headers(ResultHeaders), <<"\r\n">>, UserBody],
    io:format("response: ~p~n", [iolist_to_binary(Response)]),
    ok = gen_tcp:send(Socket, Response),
    ok.

get_headers(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {http, _, {http_request, Method, Path, _Version}} ->
            {Method, Path, get_headers(Socket, [], 0)}
    end.
get_headers(Socket, Headers, HeadersCount) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {http, _, http_eoh} ->
            Headers;
        {http, _, {http_header, _, Key, _, Value}} ->
            get_headers(Socket, [{Key, Value} | Headers], HeadersCount + 1)
    end.

get_body(Socket, Headers) ->
    ContentLength = proplists:get_value('Content-Length', Headers),
    inet:setopts(Socket, [{active, false}, {packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, ?l2i(ContentLength)),
    Body.


user_callback(_Method, _Path, _Headers, _Body) ->
    {ok, [], <<"foobar">>}.


encode_headers([]) ->
    [];
encode_headers([{K, V} | H]) ->
    [atom_to_binary(K, latin1), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].

encode_value(I) when is_integer(I) -> ?i2l(I).
