-module(elli_acceptor).
-include("elli.hrl").

-export([start_link/3, accept/3, handle_request/2, chunk_loop/3]).

start_link(Server, ListenSocket, Callback) ->
    spawn_link(?MODULE, accept, [Server, ListenSocket, Callback]).

%% @doc: Accept on the socket until a client connects. Handles the
%% request and loops if we're using keep alive or chunked transfer.
accept(Server, ListenSocket, Callback) ->
    case catch gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ?MODULE:handle_request(Socket, Callback),
            exit(normal);
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket, Callback);
        {error, closed} ->
            exit(normal)
    end.

%% @doc: Handle a HTTP request that will possibly come on the
%% socket. If nothing happens within the keep alive timeout, the
%% connection is closed.
handle_request(Socket, Callback) ->
    AcceptStart = now(),
    {Method, Path, Args, Version} = get_request(Socket),       RequestStart = now(),
    Headers                       = get_headers(Socket),       HeadersEnd = now(),
    Body                          = get_body(Socket, Headers), BodyEnd = now(),

    Req = #req{method = Method, path = Path, args = Args, version = Version,
               headers = Headers, body = Body,
               pid = self()},

    case elli_request:handle(Req, Callback) of
        {Response, chunked} ->
            ok = gen_tcp:send(Socket, Response),
            inet:setopts(Socket, [{active, once}]),
            chunk_loop(Socket, Req),
            Callback:request_complete(Req, Response, AcceptStart, RequestStart,
                                      HeadersEnd, BodyEnd, now(), now()),
            ok;
        {Response, keep_alive} ->
            UserEnd = now(),
            ok = gen_tcp:send(Socket, Response),
            Callback:request_complete(Req, Response, AcceptStart, RequestStart,
                                      HeadersEnd, BodyEnd, UserEnd, now()),
            ?MODULE:handle_request(Socket, Callback);
        {Response, close} ->
            UserEnd = now(),
            ok = gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket),
            Callback:request_complete(Req, Response, AcceptStart, RequestStart,
                                      HeadersEnd, BodyEnd, UserEnd, now()),
            exit(normal)
    end.


chunk_loop(Socket, Req) ->
    ?MODULE:chunk_loop(Socket, Req, open).

chunk_loop(Socket, Req, SocketState) ->
    receive
        {tcp_closed, Socket} ->
            io:format("client closed socket~n"),
            ?MODULE:chunk_loop(Socket, Req, closed);

        {chunk, <<>>, From} when SocketState =:= open ->
            ok = gen_tcp:send(Socket, <<"0\r\n\r\n">>),
            gen_tcp:close(Socket),
            From ! {self(), ok};

        {chunk, Data, From} when SocketState =:= open ->
            Size = integer_to_list(iolist_size(Data), 16),
            Response = [Size, <<"\r\n">>, Data, <<"\r\n">>],
            From ! {self(), gen_tcp:send(Socket, Response)},
            ?MODULE:chunk_loop(Socket, Req, SocketState);

        {chunk, _, From} when SocketState =:= closed ->
            From ! {self(), {error, closed}}

    after 1000 ->
            ?MODULE:chunk_loop(Socket, Req, SocketState)
    end.



get_request(Socket) ->
    inet:setopts(Socket, [{packet, http_bin}, {active, once}]),
    receive
        {http, _, {http_request, Method, Path, Version}} ->
            {URI, Args} = parse_path(Path),
            {Method, URI, Args, Version}
    after 50000 ->
            io:format("keep alive timeout, closing~n"),
            gen_tcp:close(Socket),
            exit(normal)
    end.



parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URI]       -> {URI, <<>>};
        [URI, Args] -> {URI, Args}
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
