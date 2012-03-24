-module(elli_http).
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
handle_request(Socket, {Mod, Args} = Callback) ->
    t(accept_start),
    {Method, RawPath, V, B0} = get_request(Socket, Callback),     t(request_start),
    {RequestHeaders, B1} = get_headers(Socket, V, B0, Callback),  t(headers_end),
    RequestBody = get_body(Socket, RequestHeaders, B1, Callback), t(body_end),

    {URL, URLArgs} = parse_path(RawPath),
    Req = #req{method = Method, path = URL, args = URLArgs, version = V,
               raw_path = RawPath,
               headers = RequestHeaders, body = RequestBody,
               pid = self(), peer = get_peer(Socket, RequestHeaders)},

    case execute_callback(Req, Callback) of
        {ResponseCode, UserHeaders, UserBody} ->
            t(user_end),
            {Body, Encoding} = encode_body(UserBody, Req),

            ResponseHeaders = [
                               connection(Req, UserHeaders),
                               content_length(Body),
                               content_encoding(Encoding)
                               | UserHeaders
                              ],
            send_response(Socket, ResponseCode, ResponseHeaders, Body, Callback),

            t(request_end),
            Mod:request_complete(Req, ResponseCode, ResponseHeaders, Body,
                                 get_timings(), Args),

            case close_or_keepalive(Req, UserHeaders) of
                keep_alive ->
                    ?MODULE:handle_request(Socket, Callback);
                close ->
                    gen_tcp:close(Socket),
                    ok
            end;

        {chunk, UserHeaders} ->
            t(user_end),

            ResponseHeaders = [{<<"Transfer-Encoding">>, <<"chunked">>},
                               connection(Req, UserHeaders)
                               | UserHeaders],
            send_response(Socket, 200, ResponseHeaders, <<>>, Callback),

            ok = chunk_loop(Socket, Req),

            t(request_end),
            Mod:request_complete(Req, 200, ResponseHeaders, <<>>, get_timings(), Args),
            ok
    end.


%% @doc: Generates a HTTP response and sends it to the client
send_response(Socket, Code, Headers, Body, {Mod, Args}) ->
    Response = [responsecode2bin(Code), <<"\r\n">>,
                encode_headers(Headers), <<"\r\n">>,
                Body],

    case gen_tcp:send(Socket, Response) of
        ok -> ok;
        {error, closed} ->
            Mod:client_closed(before_response, Args),
            ok
    end.

%% @doc: Executes the user callback, translating failure into a proper
%% response.
execute_callback(Req, {Mod, Args}) ->
    try Mod:handle(Req, Args) of
        {ok, Headers, Body}       -> {200, Headers, Body};
        {HttpCode, Headers, Body} -> {HttpCode, Headers, Body};
        {chunk, Headers}          -> {chunk, Headers}
    catch
        throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
            {ResponseCode, Headers, Body};
        throw:Exception ->
            Mod:request_throw(Req, Exception, erlang:get_stacktrace(), Args),
            {500, [], <<"Internal server error">>};
        error:Error ->
            Mod:request_error(Req, Error, erlang:get_stacktrace(), Args),
            {500, [], <<"Internal server error">>};
        exit:Exit ->
            Mod:request_exit(Req, Exit, erlang:get_stacktrace(), Args),
            {500, [], <<"Internal server error">>}
    end.



responsecode2bin(200) -> <<"HTTP/1.1 200 OK">>;
responsecode2bin(304) -> <<"HTTP/1.1 304 Not Modified">>;
responsecode2bin(403) -> <<"HTTP/1.1 403 Forbidden">>;
responsecode2bin(404) -> <<"HTTP/1.1 404 Not Found">>;
responsecode2bin(500) -> <<"HTTP/1.1 500 Internal Server Error">>.



%% @doc: The chunk loop is an intermediary between the socket and the
%% user. We forward anythingthe user sends until the user sends an
%% empty response, which signals that the connection should be
%% closed. When the client closes the socket, the loop sticks around
%% until the user tries to send something again so we can notify about
%% the closed socket.
chunk_loop(Socket, Req) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:chunk_loop(Socket, Req, open).

chunk_loop(Socket, Req, open) ->
    receive
        {tcp_closed, Socket} ->
            io:format("client closed socket~n"),
            ?MODULE:chunk_loop(Socket, Req, closed);

        {chunk, <<>>, From} ->
            ok = gen_tcp:send(Socket, <<"0\r\n\r\n">>),
            gen_tcp:close(Socket),
            From ! {self(), ok},
            ok;

        {chunk, Data, From} ->
            Size = integer_to_list(iolist_size(Data), 16),
            Response = [Size, <<"\r\n">>, Data, <<"\r\n">>],
            From ! {self(), gen_tcp:send(Socket, Response)},
            ?MODULE:chunk_loop(Socket, Req, open)
    after 1000 ->
            ?MODULE:chunk_loop(Socket, Req, open)
    end;

chunk_loop(Socket, Req, closed) ->
    receive
        {chunk, _, From} ->
            From ! {self(), {error, closed}}
    after 1000 ->
            ?MODULE:chunk_loop(Socket, Req, closed)
    end.



get_request(Socket, Callback) ->
    get_request(Socket, <<>>, Callback).

get_request(Socket, Buffer, {Mod, Args} = Callback) ->
    case gen_tcp:recv(Socket, 0, 60000) of
        {ok, Data} ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            case erlang:decode_packet(http_bin, NewBuffer, []) of
                {ok, {http_request, Method, RawPath, Version}, Rest} ->
                    {Method, RawPath, Version, Rest};
                {ok, {http_error, _}, _} ->
                    Mod:request_parse_error(NewBuffer, Args),
                    error_terminate(400, Socket);
                {more, _} ->
                    get_request(Socket, NewBuffer, Callback)
            end;
        {error, timeout} ->
            gen_tcp:close(Socket),
            exit(normal);
        {error, closed} ->
            gen_tcp:close(Socket),
            exit(normal)
    end.


error_terminate(_Code, Socket) ->
    gen_tcp:send(Socket, [<<"400 Bad Request">>]),
    gen_tcp:close(Socket),
    exit(normal).


t(Key) ->
    put({time, Key}, now()).

get_timings() ->
    lists:flatmap(fun ({{time, Key}, Val}) ->
                          erase({time, Key}),
                          [{Key, Val}];
                     (_) ->
                          []
                 end, get()).


parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URL]       -> {split_path(URL), []};
        [URL, Args] -> {split_path(URL), split_args(Args)}
    end.

split_path(<<"/", Path/binary>>) ->
    binary:split(Path, [<<"/">>], [global, trim]);
split_path(Path) ->
    binary:split(Path, [<<"/">>], [global, trim]).




%% @doc: Splits the url arguments into a proplist. Lifted from
%% cowboy_http:x_www_form_urlencoded/2
-spec split_args(binary()) -> list({binary(), binary() | true}).
split_args(<<>>) ->
	[];
split_args(Qs) ->
	Tokens = binary:split(Qs, <<"&">>, [global, trim]),
	[case binary:split(Token, <<"=">>) of
		[Token] -> {Token, true};
		[Name, Value] -> {Name, Value}
	end || Token <- Tokens].

%%
%% RECEIVE REQUEST
%%

-spec get_headers(port(), term(), binary(), callback()) -> headers().
get_headers(_Socket, {0, 9}, _, _) ->
    {[], <<>>};
get_headers(Socket, {1, _}, Buffer, Callback) ->
    get_headers(Socket, Buffer, [], 0, Callback).

get_headers(Socket, Buffer, Headers, HeadersCount, {Mod, Args} = Callback) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, Key, _, Value}, Rest} ->
            NewHeaders = [{ensure_binary(Key), Value} | Headers],
            get_headers(Socket, Rest, NewHeaders, HeadersCount + 1, Callback);
        {ok, http_eoh, Rest} ->
            {Headers, Rest};
        {more, _} ->
            case gen_tcp:recv(Socket, 0, 10000) of
                {ok, Data} ->
                    get_headers(Socket, <<Buffer/binary, Data/binary>>,
                                Headers, HeadersCount, Callback);
                {error, closed} ->
                    Mod:client_closed(receiving_headers, Args),
                    gen_tcp:close(Socket),
                    exit(normal);
                {error, timeout} ->
                    Mod:client_timeout(receiving_headers, Args),
                    gen_tcp:close(Socket),
                    exit(normal)
            end
    end.

-spec get_body(port(), headers(), binary(), callback) -> body().
get_body(Socket, Headers, Buffer, {Mod, Args}) ->
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
                        {error, closed} ->
                            Mod:client_closed(receiving_body, Args),
                            ok = gen_tcp:close(Socket),
                            exit(normal);
                        {error, timeout} ->
                            Mod:client_timeout(receiving_body, Args),
                            ok = gen_tcp:close(Socket),
                            exit(normal)
                    end
            end
    end.


ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).


%%
%% HEADERS
%%

encode_headers([]) ->
    [];

encode_headers([[] | H]) ->
    encode_headers(H);
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> ?i2l(V);
encode_value(V) when is_binary(V)  -> V.


connection_token(#req{version = {1, 1}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"close">> -> <<"close">>;
        <<"Close">> -> <<"close">>;
        _           -> <<"Keep-Alive">>
    end;

connection_token(#req{version = {1, 0}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"Keep-Alive">> -> <<"Keep-Alive">>;
        _                -> <<"close">>
    end.

close_or_keepalive(Req, UserHeaders) ->
    case proplists:get_value(<<"Connection">>, UserHeaders) of
        undefined ->
            case connection_token(Req) of
                <<"Keep-Alive">> -> keep_alive;
                <<"close">>      -> close
            end;
        <<"close">> -> close;
        <<"Keep-Alive">> -> keep_alive
    end.


%% @doc: Adds appropriate connection header if the user did not add
%% one already.
connection(Req, UserHeaders) ->
    case proplists:get_value(<<"Connection">>, UserHeaders) of
        undefined ->
            {<<"Connection">>, connection_token(Req)};
        _ ->
            []
    end.



get_peer(Socket, Headers) ->
    case proplists:get_value(<<"X-Forwarded-For">>, Headers) of
        undefined ->
            case inet:peername(Socket) of
                {ok, {Address, _}} ->
                    list_to_binary(inet_parse:ntoa(Address));
                {error, _} ->
                    undefined
            end;
        Ip ->
            Ip
    end.


content_encoding(gzip)    -> {<<"Content-Encoding">>, <<"gzip">>};
content_encoding(deflate) -> {<<"Content-Encoding">>, <<"deflate">>};
content_encoding(none)    -> [].

content_length(Body) ->
    case size(Body) of
        0 -> [];
        N -> {<<"Content-Length">>, N}
    end.


accepted_encoding(Headers) ->
    Encodings = binary:split(
                  proplists:get_value(<<"Accept-Encoding">>, Headers, <<>>),
                  [<<",">>, <<";">>], [global]),
    case Encodings of
        [E] -> E;
        [E|_] -> E
    end.

encode_body(Body, #req{headers = Headers}) ->
    case should_compress(Body) of
        true ->
            case accepted_encoding(Headers) of
                <<"gzip">>     -> {zlib:gzip(Body), gzip};
                <<"deflate">>  -> {zlib:compress(Body), deflate};
                <<"identity">> -> {Body, none};
                <<>>           -> {Body, none};
                _Other         -> {Body, none}
            end;
        false ->
            {Body, none}
    end.


should_compress(Body) when byte_size(Body) >= 1024 -> true;
should_compress(_)                                 -> false.


