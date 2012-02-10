-module(elli_request).
-include("elli.hrl").

-export([handle/5]).

-record(req, {
          method,
          path,
          args,
          headers,
          body
}).


handle(Socket, Callback, Method, FullPath, Version) ->
    {Path, Args} = get_uri(FullPath),
    RequestHeaders = get_headers(Socket),
    Body = get_body(Socket, RequestHeaders),

    Req = #req{method = Method, path = Path, args = Args,
               headers = RequestHeaders,
               body = Body},


    {ResponseCode, UserHeaders, UserBody} = execute_callback(Callback, Req),

    ResponseHeaders = [
                       {<<"Content-Length">>, size(UserBody)},
                       {<<"Connection">>, connection_token(Version, RequestHeaders)}
                       | UserHeaders],
    %% io:format("response headers: ~p~n", [ResponseHeaders]),

    Response = [responsecode2bin(ResponseCode), <<"\r\n">>,
                encode_headers(ResponseHeaders), <<"\r\n">>,
                UserBody],

    {Response, connection_token(Version, RequestHeaders)}.


responsecode2bin(200) -> <<"HTTP/1.1 200 OK">>;
responsecode2bin(500) -> <<"HTTP/1.1 500 Internal Server Error">>.


execute_callback(C, Req) ->
    try C(Req) of
        {ok, Headers, Body}       -> {200, Headers, Body};
        {HttpCode, Headers, Body} -> {HttpCode, Headers, Body}
    catch
        exit:Exit ->
            io:format("crash: ~p~n", [Exit]),
            {500, [], <<>>}
    end.


get_uri(FullPath) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URI]       -> {URI, <<>>};
        [URI, Args] -> {URI, Args}
    end.


connection_token({1, 1}, Headers) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"close">> -> <<"close">>;
        _           -> <<"Keep-Alive">>
    end;

connection_token({1, 0}, Headers) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"Keep-Alive">> -> <<"Keep-Alive">>;
        _                -> <<"close">>
    end.

get_headers(Socket) ->
    get_headers(Socket, [], 0).

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



encode_headers([]) ->
    [];
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> ?i2l(V);
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_atom(V)    -> atom_to_binary(V, latin1).
