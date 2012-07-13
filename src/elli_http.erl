%% @doc: Elli HTTP request implementation
%%
%% An elli_http process blocks in gen_tcp:accept/2 until a client
%% connects. It then handles requests on that connection until it's
%% closed either by the client timing out or explicitly by the user.
-module(elli_http).
-include("elli.hrl").

-export([start_link/3, accept/3, handle_request/2, chunk_loop/3,
         split_args/1, parse_path/1]).

-spec start_link(pid(), port(), callback()) -> pid().
start_link(Server, ListenSocket, Callback) ->
    spawn_link(?MODULE, accept, [Server, ListenSocket, Callback]).

-spec accept(pid(), port(), callback()) -> ok.
%% @doc: Accept on the socket until a client connects. Handles the
%% request and loops if we're using keep alive or chunked transfer.
accept(Server, ListenSocket, Callback) ->
    case catch gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            t(accepted),
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
    {Method, RawPath, V, B0} = get_request(Socket, Callback),     t(request_start),
    {RequestHeaders, B1} = get_headers(Socket, V, B0, Callback),  t(headers_end),
    RequestBody = get_body(Socket, RequestHeaders, B1, Callback), t(body_end),

    {Path, URL, URLArgs} = parse_path(RawPath),
    Req = #req{method = Method, path = URL, args = URLArgs, version = V,
               raw_path = Path,
               headers = RequestHeaders, body = RequestBody,
               pid = self(), peer = get_peer(Socket, RequestHeaders)},

    t(user_start),
    case execute_callback(Req, Callback) of
        {response, ResponseCode, UserHeaders, UserBody} ->
            t(user_end),

            ResponseHeaders = [
                               connection(Req, UserHeaders),
                               content_length(UserBody)
                               | UserHeaders
                              ],
            send_response(Socket, ResponseCode, ResponseHeaders, UserBody, Callback),

            t(request_end),
            Mod:handle_event(request_complete,
                             [Req, ResponseCode, ResponseHeaders, UserBody, get_timings()],
                             Args),

            case close_or_keepalive(Req, UserHeaders) of
                keep_alive ->
                    ?MODULE:handle_request(Socket, Callback);
                close ->
                    gen_tcp:close(Socket),
                    ok
            end;

        {chunk, UserHeaders, Initial} ->
            t(user_end),

            ResponseHeaders = [{<<"Transfer-Encoding">>, <<"chunked">>},
                               connection(Req, UserHeaders)
                               | UserHeaders],
            send_response(Socket, 200, ResponseHeaders, <<"">>, Callback),
            Initial =:= <<"">> orelse send_chunk(Socket, Initial),

            chunk_loop(Socket, Req),

            t(request_end),
            Mod:handle_event(request_complete,
                             [Req, 200, ResponseHeaders, <<>>, get_timings()],
                             Args),
            ok
    end.


%% @doc: Generates a HTTP response and sends it to the client
send_response(Socket, Code, Headers, Body, {Mod, Args}) ->
    Response = [<<"HTTP/1.1 ">>, status(Code), <<"\r\n">>,
                encode_headers(Headers), <<"\r\n">>,
                Body],

    case gen_tcp:send(Socket, Response) of
        ok -> ok;
        {error, closed} ->
            Mod:handle_event(client_closed, [before_response], Args),
            ok
    end.

%% @doc: Executes the user callback, translating failure into a proper
%% response.
execute_callback(Req, {Mod, Args}) ->
    try Mod:handle(Req, Args) of
        {ok, Headers, Body}       -> {response, 200, Headers, Body};
        {ok, Body}                -> {response, 200, [], Body};
        {chunk, Headers}          -> {chunk, Headers, <<"">>};
        {chunk, Headers, Initial} -> {chunk, Headers, Initial};
        {HttpCode, Headers, Body} -> {response, HttpCode, Headers, Body};
        {HttpCode, Body}          -> {response, HttpCode, [], Body}
    catch
        throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
            {response, ResponseCode, Headers, Body};
        throw:Exception ->
            Mod:handle_event(request_throw, [Req, Exception, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>};
        error:Error ->
            Mod:handle_event(request_error, [Req, Error, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>};
        exit:Exit ->
            Mod:handle_event(request_exit, [Req, Exit, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>}
    end.


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
            ?MODULE:chunk_loop(Socket, Req, closed);

        {chunk, <<>>} ->
            gen_tcp:send(Socket, <<"0\r\n\r\n">>),
            gen_tcp:close(Socket),
            ok;
        {chunk, <<>>, From} ->
            case gen_tcp:send(Socket, <<"0\r\n\r\n">>) of
                ok ->
                    gen_tcp:close(Socket),
                    From ! {self(), ok},
                    ok;
                {error, closed} ->
                    From ! {self(), {error, closed}},
                    ok
            end;

        {chunk, Data} ->
            send_chunk(Socket, Data),
            ?MODULE:chunk_loop(Socket, Req, open);
        {chunk, Data, From} ->
            case send_chunk(Socket, Data) of
                ok ->
                    From ! {self(), ok};
                {error, closed} ->
                    From ! {self(), {error, closed}}
            end,
            ?MODULE:chunk_loop(Socket, Req, open)
    after 10000 ->
            ?MODULE:chunk_loop(Socket, Req, open)
    end;

chunk_loop(Socket, Req, closed) ->
    receive
        {chunk, _, From} ->
            From ! {self(), {error, closed}}
    after 10000 ->
            ?MODULE:chunk_loop(Socket, Req, closed)
    end.


send_chunk(Socket, Data) ->
    Size = integer_to_list(iolist_size(Data), 16),
    Response = [Size, <<"\r\n">>, Data, <<"\r\n">>],
    gen_tcp:send(Socket, Response).



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
                    Mod:handle_event(request_parse_error, [NewBuffer], Args),
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
    put({time, Key}, os:timestamp()).

get_timings() ->
    lists:flatmap(fun ({{time, Key}, Val}) ->
                          if
                              Key =:= accepted -> ok;
                              true -> erase({time, Key})
                          end,
                          [{Key, Val}];
                     (_) ->
                          []
                 end, get()).


parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URL]       -> {FullPath, split_path(URL), []};
        [URL, Args] -> {FullPath, split_path(URL), split_args(Args)}
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

-spec get_headers(port(), version(), binary(), callback()) ->
                         {headers(), any()}.
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
                    Mod:handle_event(client_closed, [receiving_headers], Args),
                    gen_tcp:close(Socket),
                    exit(normal);
                {error, timeout} ->
                    Mod:handle_event(client_timeout, [receiving_headers], Args),
                    gen_tcp:close(Socket),
                    exit(normal)
            end
    end.

-spec get_body(port(), headers(), binary(), callback()) -> body().
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
                            Mod:handle_event(client_closed, [receiving_body], Args),
                            ok = gen_tcp:close(Socket),
                            exit(normal);
                        {error, timeout} ->
                            Mod:handle_event(client_timeout, [receiving_body], Args),
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
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_list(V) -> list_to_binary(V).


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


content_length(Body)->
    {<<"Content-Length">>, iolist_size(Body)}.



%% @doc: Response code string. Lifted from cowboy_http_req.erl
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(B) when is_binary(B) -> B.
