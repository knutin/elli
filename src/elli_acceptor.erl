-module(elli_acceptor).
-include("elli.hrl").

-export([start_link/3, accept/3, handle_request/2, chunk_loop/3]).

-spec start_link(pid(), port(), callback()) -> pid().
start_link(Server, ListenSocket, Callback) ->
    spawn_link(?MODULE, accept, [Server, ListenSocket, Callback]).

-spec accept(pid(), port(), callback()) -> ok.
%% @doc: Accept on the socket until a client connects. Handles the
%% request and loops if we're using keep alive or chunked transfer.
accept(Server, ListenSocket, Callback) ->
    case catch gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ?MODULE:handle_request(Socket, Callback),
            ok;
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket, Callback);
        {error, closed} ->
            ok
    end.

-spec handle_request(port(), callback()) -> ok.
%% @doc: Handle a HTTP request that will possibly come on the
%% socket. If nothing happens within the keep alive timeout, the
%% connection is closed.
handle_request(Socket, {CallbackMod, CallbackArgs} = Callback) ->
    AcceptStart = now(),
    {Method, Path, Args, V, Buf0} = get_request(Socket),        RequestStart = now(),
    {Headers, Buf1}               = get_headers(Socket, V, Buf0), HeadersEnd = now(),
    Body                          = get_body(Socket, Headers, Buf1), BodyEnd = now(),

    %% Big-ip hack
    case V of
        {0, 9} ->
            gen_tcp:send(Socket, <<"Hello World!\r\n">>),
            exit(normal);
        _ ->
            ok
    end,

    Req = #req{method = Method, path = Path, args = Args, version = V,
               headers = Headers, body = Body,
               pid = self()},

    case elli_request:handle(Req, Callback) of
        {Response, chunked} ->
            ok = gen_tcp:send(Socket, Response),
            inet:setopts(Socket, [{active, once}]),
            chunk_loop(Socket, Req),
            CallbackMod:request_complete(Req, Response, AcceptStart, RequestStart,
                                      HeadersEnd, BodyEnd, now(), now(), CallbackArgs),
            ok;
        {Response, keep_alive} ->
            UserEnd = now(),
            send_response(Socket, Response),
            CallbackMod:request_complete(Req, Response, AcceptStart, RequestStart,
                                      HeadersEnd, BodyEnd, UserEnd, now(), CallbackArgs),
            ?MODULE:handle_request(Socket, Callback);
        {Response, close} ->
            UserEnd = now(),
            ok = gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket),
            CallbackMod:request_complete(Req, Response, AcceptStart, RequestStart,
                                         HeadersEnd, BodyEnd, UserEnd, now(), CallbackArgs),
            ok
    end.

send_response(Socket, Response) ->
    case gen_tcp:send(Socket, Response) of
        ok -> ok;
        {error, closed} ->
            error_logger:info_msg("client closed connection before we could send response~n"),
            ok
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
    get_request(Socket, <<>>).

get_request(Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0, 60000) of
        {ok, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case erlang:decode_packet(http_bin, NewBuffer, []) of
                {ok, {http_request, Method, Path, Version}, Rest} ->
                    {URI, Args} = parse_path(Path),
                    {Method, URI, Args, Version, Rest};
                {ok, {http_error, Bin}, Rest} ->
                    error_logger:info_msg("Error parsing request: ~p~n", [{Bin, Rest, NewBuffer}]),
                    error_terminate(400, Socket);
                {more, _} ->
                    get_request(Socket, NewBuffer)
            end;
        {error, timeout} ->
            %%io:format("keep alive timeout, closing~n"),
            gen_tcp:close(Socket),
            exit(normal);
        {error, closed} ->
            %%io:format("socket closed while waiting for request~n"),
            ok = gen_tcp:close(Socket),
            exit(normal)
    end.


error_terminate(_Code, Socket) ->
    gen_tcp:send(Socket, [<<"400 Bad Request">>]),
    gen_tcp:close(Socket),
    exit(normal).


parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URI]       -> {URI, <<>>};
        [URI, Args] -> {URI, Args}
    end.

-spec get_headers(port(), term(), binary()) -> headers().
get_headers(_Socket, {0, 9}, _) ->
    {[], <<>>};
get_headers(Socket, {1, _}, Buffer) ->
    get_headers(Socket, Buffer, [], 0).

get_headers(Socket, Buffer, Headers, HeadersCount) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, Key, _, Value}, Rest} ->
            NewHeaders = [{ensure_binary(Key), Value} | Headers],
            get_headers(Socket, Rest, NewHeaders, HeadersCount + 1);
        {ok, http_eoh, Rest} ->
            {Headers, Rest};
        {more, _} ->
            case gen_tcp:recv(Socket, 0, 10000) of
                {ok, Data} ->
                    get_headers(Socket, <<Buffer/binary, Data/binary>>,
                                Headers, HeadersCount);
                {error, timeout} ->
                    error_logger:info_msg("timeout in getting headers~n"),
                    gen_tcp:close(Socket),
                    exit(normal)
            end
    end.

-spec get_body(port(), headers(), binary()) -> body().
get_body(Socket, Headers, Buffer) ->
    case proplists:get_value(<<"Content-Length">>, Headers, undefined) of
        undefined ->
            <<>>;
        ContentLengthBin ->
            ContentLength = ?b2i(ContentLengthBin),
            BufSize = byte_size(Buffer),

            case ContentLength - BufSize of
                0 ->
                    Buffer;
                N ->
                    case gen_tcp:recv(Socket, N, 30000) of
                        {ok, Data} ->
                            <<Buffer/binary, Data/binary>>;
                        {error, timeout} ->
                            ok = gen_tcp:close(Socket),
                            %%error_logger:info_msg("recv body timeout~n"),
                            exit(normal);
                        {error, closed} ->
                            ok = gen_tcp:close(Socket),
                            exit(normal)
                    end
            end
    end.


ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom)-> atom_to_binary(Atom, latin1).

