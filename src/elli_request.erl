-module(elli_request).
-include("elli.hrl").

-export([handle/1]).

handle(Socket) ->
    {Method, Path, Version, Headers} = get_headers(Socket),
    Body = get_body(Socket, Headers),

    {ok, UserHeaders, UserBody} = user_callback(Method, Path, Headers, Body),

    ResultHeaders = [{<<"Connection">>, <<"Keep-Alive">>} | [{'Content-Length', size(UserBody)} | UserHeaders]],

    Response = [<<"HTTP/1.1 200 OK\r\n">>,
                encode_headers(ResultHeaders), <<"\r\n">>, UserBody],
    ok = gen_tcp:send(Socket, Response),

    connection_token(Version, Headers).

connection_token({1, 1}, Headers) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"close">> -> close;
        _           -> keep_alive
    end;

connection_token({1, 0}, Headers) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"Keep-Alive">> -> keep_alive;
        _                -> close
    end.

get_headers(Socket) ->
    inet:setopts(Socket, [{packet, http_bin}, {active, once}]),
    receive
        {http, _, {http_request, Method, Path, Version}} ->
            {Method, Path, Version, get_headers(Socket, [], 0)}
    end.
get_headers(Socket, Headers, HeadersCount) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {http, _, http_eoh} ->
            Headers;
        {http, _, {http_header, _, Key, _, Value}} ->
            get_headers(Socket, [{atom_to_binary(Key, latin1), Value} | Headers],
                        HeadersCount + 1)
    end.

get_body(Socket, Headers) ->
    case proplists:get_value('Content-Length', Headers, undefined) of
        undefined ->
            <<>>;
        ContentLength ->
            inet:setopts(Socket, [{active, false}, {packet, raw}]),
            {ok, Body} = gen_tcp:recv(Socket, ?l2i(ContentLength)),
            Body
    end.


user_callback(_Method, _Path, _Headers, _Body) ->
    {ok, [], <<"foobar">>}.


encode_headers([]) ->
    [];
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> ?i2l(V);
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_atom(V)    -> atom_to_binary(V, latin1).
