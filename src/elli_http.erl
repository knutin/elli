%% @doc: Elli HTTP request implementation
%%
%% An elli_http process blocks in gen_tcp:accept/2 until a client
%% connects. It then handles requests on that connection until it's
%% closed either by the client timing out or explicitly by the user.
-module(elli_http).
-include("../include/elli.hrl").

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/4]).

-export([mk_req/7]). %% useful when testing.

%% Exported for looping with a fully-qualified module name
-export([accept/4, handle_request/4, chunk_loop/1, split_args/1,
         parse_path/1, keepalive_loop/3, keepalive_loop/5]).


-spec start_link(pid(), port(), proplists:proplist(), callback()) -> pid().
start_link(Server, ListenSocket, Options, Callback) ->
    spawn_link(?MODULE, accept, [Server, ListenSocket, Options, Callback]).

-spec accept(pid(), port(), proplists:proplist(), callback()) -> ok.
%% @doc: Accept on the socket until a client connects. Handles the
%% request, then loops if we're using keep alive or chunked
%% transfer. If accept doesn't give us a socket within 10 seconds, we
%% loop to allow code upgrades.
accept(Server, ListenSocket, Options, Callback) ->
    case catch gen_tcp:accept(ListenSocket, accept_timeout(Options)) of
        {ok, Socket} ->
            t(accepted),
            gen_server:cast(Server, accepted),
            ?MODULE:keepalive_loop(Socket, Options, Callback);
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket, Options, Callback);
        {error, econnaborted} ->
            ?MODULE:accept(Server, ListenSocket, Options, Callback);
        {error, closed} ->
            ok;
        {error, Other} ->
            exit({error, Other})
    end.


%% @doc: Handle multiple requests on the same connection, ie. "keep
%% alive".
keepalive_loop(Socket, Options, Callback) ->
    keepalive_loop(Socket, 0, <<>>, Options, Callback).

keepalive_loop(Socket, NumRequests, Buffer, Options, Callback) ->
    case ?MODULE:handle_request(Socket, Buffer, Options, Callback) of
        {keep_alive, NewBuffer} ->
            ?MODULE:keepalive_loop(Socket, NumRequests, NewBuffer, Options, Callback);
        {close, _} ->
            gen_tcp:close(Socket),
            ok
    end.

-spec handle_request(port(), binary(), proplists:proplist(), callback()) ->
                            {'keep_alive' | 'close', binary()}.
%% @doc: Handle a HTTP request that will possibly come on the
%% socket. Returns the appropriate connection token and any buffer
%% containing (parts of) the next request.
handle_request(S, PrevB, Opts, {Mod, Args} = Callback) ->
    {Method, RawPath, V, B0} = get_request(S, PrevB, Opts, Callback), t(request_start),
    {RequestHeaders, B1} = get_headers(S, V, B0, Opts, Callback),       t(headers_end),
    {RequestBody, B2} = get_body(S, RequestHeaders, B1, Opts, Callback),   t(body_end),

    Req = mk_req(Method, RawPath, RequestHeaders, RequestBody, V, S, Callback),

    t(user_start),
    case execute_callback(Req, Callback) of
        {response, ResponseCode, UserHeaders, UserBody} ->
            t(user_end),

            ResponseHeaders = [connection(Req, UserHeaders),
                               content_length(UserHeaders, UserBody)
                               | UserHeaders],
            send_response(S, Method, ResponseCode,
                          ResponseHeaders, UserBody, Callback),

            t(request_end),
            Mod:handle_event(request_complete,
                             [Req, ResponseCode, ResponseHeaders, UserBody, get_timings()],
                             Args),

            {close_or_keepalive(Req, UserHeaders), B2};

        {chunk, UserHeaders, Initial} ->
            t(user_end),

            ResponseHeaders = [{<<"Transfer-Encoding">>, <<"chunked">>},
                               connection(Req, UserHeaders)
                               | UserHeaders],
            send_response(S, Method, 200, ResponseHeaders, <<"">>, Callback),
            Initial =:= <<"">> orelse send_chunk(S, Initial),

            ClosingEnd = case start_chunk_loop(S) of
                             {error, client_closed} -> client;
                             ok -> server
                         end,

            t(request_end),
            Mod:handle_event(chunk_complete,
                             [Req, 200, ResponseHeaders, ClosingEnd, get_timings()],
                             Args),
            {close, <<>>};

        {file, ResponseCode, UserHeaders, Filename, FileOpts} ->
            t(user_end),

            ResponseHeaders = [connection(Req, UserHeaders) | UserHeaders],
            send_file(S, RequestHeaders, ResponseCode, ResponseHeaders, Filename, FileOpts, Callback),

            t(request_end),

            Mod:handle_event(request_complete,
                             [Req, ResponseCode, ResponseHeaders, <<>>, get_timings()],
                             Args),

            {close_or_keepalive(Req, UserHeaders), B2}
    end.

-spec mk_req(Method::http_method(), RawPath::binary(), RequestHeaders::headers(),
             RequestBody::body(), V::version(), Socket::inet:socket(),
             Callback::callback()) -> record(req).
mk_req(Method, RawPath, RequestHeaders, RequestBody, V, Socket, Callback) ->
    {Mod, Args} = Callback,
    case parse_path(RawPath) of
        {ok, {Path, URL, URLArgs}} ->
            #req{method = Method, path = URL, args = URLArgs, version = V,
                 raw_path = Path, headers = RequestHeaders,
                 body = RequestBody, pid = self(), socket = Socket};
        {error, Reason} ->
            Mod:handle_event(request_parse_error,
                    [{Reason, {Method, RawPath}}], Args),
            send_bad_request(Socket),
            gen_tcp:close(Socket),
            exit(normal)
    end.


%% @doc: Generates a HTTP response and sends it to the client
send_response(Socket, Method, Code, Headers, UserBody, {Mod, Args}) ->
    Body = case {Method, Code} of
               {'HEAD', _} -> <<>>;
               {_, 304}    -> <<>>;
               {_, 204}    -> <<>>;
               _           -> UserBody
           end,

    Response = [<<"HTTP/1.1 ">>, status(Code), <<"\r\n">>,
                encode_headers(Headers), <<"\r\n">>,
                Body],

    case gen_tcp:send(Socket, Response) of
        ok -> ok;
        {error, closed} ->
            Mod:handle_event(client_closed, [before_response], Args),
            ok
    end.


-spec send_file(Socket::inet:socket(), RequestHeaders::headers(),
                Code::response_code(), Headers::headers(), Filename::file:filename(),
                Opts::proplists:proplist(), Callback::callback()) -> ok.

%% @doc: Generates and sends a HTTP response to the client where the body
%% is the contents of the given file. Supports single byte-range requests.
send_file(Socket, RequestHeaders, Code, Headers, Filename, Opts, {Mod, Args}) ->
    Size = case proplists:get_value(size, Opts) of
               undefined -> get_size(Filename);
               Size0     -> Size0
           end,
    Range = case proplists:get_value(range, Opts) of
                undefined -> get_range(RequestHeaders, Size);
                Range0    -> Range0
            end,
    Headers0 = set_range_and_length(Range, Size, Headers),
    %% Rewrite status code when a (in)valid range is present.
    Code0 = case Range of
                undefined when Size =:= 0, Code =:= 200 -> 204;
                undefined -> Code;
                {_,_}     -> 206;
                invalid   -> 416
            end,

    ResponseHeaders = [<<"HTTP/1.1 ">>, status(Code0), <<"\r\n">>,
                       encode_headers(Headers0), <<"\r\n">>],
    case gen_tcp:send(Socket, ResponseHeaders) of
        ok ->
            case send_file(Socket, Filename, Size, Range) of
                {ok, _BytesSent} -> ok;
                {error, closed} ->
                    Mod:handle_event(client_closed, [before_response], Args),
                    ok;
                {error, invalid_range} ->
                    Mod:handle_event(requested_range_not_satisfiable, [], Args),
                    ok
            end;
        {error, closed} ->
            Mod:handle_event(client_closed, [before_response], Args),
            ok
    end.

%% Makes passing a length of 0 a no-op.
%% The default behaviour of file:send_file/5 makes
%% passing a length of 0 sending the complete file.
send_file(_, _, _, {_, 0}) ->
    {ok, 0};
send_file(Socket, Filename, _, {Offset, Bytes}) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, Fd} ->
            Res = file:sendfile(Fd, Socket, Offset, Bytes, []),
            file:close(Fd),
            Res;
        {error, Reason} ->
            {error, Reason}
    end;
send_file(Socket, Filename, Size, undefined) ->
    send_file(Socket, Filename, Size, {0, Size});
%% Don't send file when range is invalid.
send_file(_, _, _, invalid) ->
    {error, invalid_range}.

get_size(Filename) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size = Size}} -> Size;
        _ -> 0
    end.

send_bad_request(Socket) ->
    Response = [<<"HTTP/1.1 ">>, status(400), <<"\r\n">>],
    gen_tcp:send(Socket, Response).

%% @doc: Executes the user callback, translating failure into a proper
%% response.
execute_callback(Req, {Mod, Args}) ->
    try Mod:handle(Req, Args) of
        {ok, Headers, {file, Filename}}       -> {file, 200, Headers, Filename, []};
        {ok, Headers, {file, Filename, Opts}} -> {file, 200, Headers, Filename, Opts};
        {ok, Headers, Body}                   -> {response, 200, Headers, Body};
        {ok, Body}                            -> {response, 200, [], Body};
        {chunk, Headers}                      -> {chunk, Headers, <<"">>};
        {chunk, Headers, Initial}             -> {chunk, Headers, Initial};
        {HttpCode, Headers, {file, Filename}} -> {file, HttpCode, Headers, Filename, []};
        {HttpCode, Headers, {file, Filename, Opts}} ->
                                                 {file, HttpCode, Headers, Filename, Opts};
        {HttpCode, Headers, Body}             -> {response, HttpCode, Headers, Body};
        {HttpCode, Body}                      -> {response, HttpCode, [], Body}
    catch
        throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
            {response, ResponseCode, Headers, Body};
        throw:Exc ->
            Mod:handle_event(request_throw, [Req, Exc, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>};
        error:Error ->
            Mod:handle_event(request_error, [Req, Error, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>};
        exit:Exit ->
            Mod:handle_event(request_exit, [Req, Exit, erlang:get_stacktrace()], Args),
            {response, 500, [], <<"Internal server error">>}
    end.


%%
%% CHUNKED-TRANSFER
%%


%% @doc: The chunk loop is an intermediary between the socket and the
%% user. We forward anything the user sends until the user sends an
%% empty response, which signals that the connection should be
%% closed. When the client closes the socket, the loop exits.
start_chunk_loop(Socket) ->
    %% Set the socket to active so we receive the tcp_closed message
    %% if the client closes the connection
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:chunk_loop(Socket).

chunk_loop(Socket) ->
    receive
        {tcp_closed, Socket} ->
            {error, client_closed};

        {chunk, <<>>} ->
            case gen_tcp:send(Socket, <<"0\r\n\r\n">>) of
                ok ->
                    gen_tcp:close(Socket),
                    ok;
                {error, closed} ->
                    {error, client_closed}
            end;
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
            ?MODULE:chunk_loop(Socket);
        {chunk, Data, From} ->
            case send_chunk(Socket, Data) of
                ok ->
                    From ! {self(), ok};
                {error, closed} ->
                    From ! {self(), {error, closed}}
            end,
            ?MODULE:chunk_loop(Socket)
    after 10000 ->
            ?MODULE:chunk_loop(Socket)
    end.


send_chunk(Socket, Data) ->
    Size = integer_to_list(iolist_size(Data), 16),
    Response = [Size, <<"\r\n">>, Data, <<"\r\n">>],
    gen_tcp:send(Socket, Response).


%%
%% RECEIVE REQUEST
%%

%% @doc: Retrieves the request line
get_request(Socket, Buffer, Options, {Mod, Args} = Callback) ->
    case erlang:decode_packet(http_bin, Buffer, []) of
        {more, _} ->
            case gen_tcp:recv(Socket, 0, request_timeout(Options)) of
                {ok, Data} ->
                    NewBuffer = <<Buffer/binary, Data/binary>>,
                    get_request(Socket, NewBuffer, Options, Callback);
                {error, timeout} ->
                    Mod:handle_event(request_timeout, [], Args),
                    gen_tcp:close(Socket),
                    exit(normal);
                {error, closed} ->
                    Mod:handle_event(request_closed, [], Args),
                    gen_tcp:close(Socket),
                    exit(normal)
            end;
        {ok, {http_request, Method, RawPath, Version}, Rest} ->
            {Method, RawPath, Version, Rest};
        {ok, {http_error, _}, _} ->
            Mod:handle_event(request_parse_error, [Buffer], Args),
            send_bad_request(Socket),
            gen_tcp:close(Socket),
            exit(normal)
    end.

-spec get_headers(port(), version(), binary(), proplists:proplist(), callback()) ->
                         {headers(), any()}.
get_headers(_Socket, {0, 9}, _, _, _) ->
    {[], <<>>};
get_headers(Socket, {1, _}, Buffer, Opts, Callback) ->
    get_headers(Socket, Buffer, [], 0, Opts, Callback).

get_headers(Socket, _, Headers, HeadersCount, _Opts, {Mod, Args})
  when HeadersCount >= 100 ->
    Mod:handle_event(bad_request, [{too_many_headers, Headers}], Args),
    send_bad_request(Socket),
    gen_tcp:close(Socket),
    exit(normal);
get_headers(Socket, Buffer, Headers, HeadersCount, Opts, {Mod, Args} = Callback) ->
    case erlang:decode_packet(httph_bin, Buffer, []) of
        {ok, {http_header, _, Key, _, Value}, Rest} ->
            NewHeaders = [{ensure_binary(Key), Value} | Headers],
            get_headers(Socket, Rest, NewHeaders, HeadersCount + 1, Opts, Callback);
        {ok, http_eoh, Rest} ->
            {Headers, Rest};
        {more, _} ->
            case gen_tcp:recv(Socket, 0, header_timeout(Opts)) of
                {ok, Data} ->
                    get_headers(Socket, <<Buffer/binary, Data/binary>>,
                                Headers, HeadersCount, Opts, Callback);
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

-spec get_body(port(), headers(), binary(),
               proplists:proplist(), callback()) -> body().
%% @doc: Fetches the full body of the request, if any is available.
%%
%% At the moment we don't need to handle large requests, so there is
%% no need for streaming or lazily fetching the body in the user
%% code. Fully receiving the body allows us to avoid the complex
%% request object threading in Cowboy and the caching in Mochiweb.
%%
%% As we are always receiving whatever the client sends, we might have
%% buffered too much and get parts of the next pipelined request. In
%% that case, push it back in the buffer and handle the first request.
get_body(Socket, Headers, Buffer, Opts, {Mod, Args} = Callback) ->
    case proplists:get_value(<<"Content-Length">>, Headers, undefined) of
        undefined ->
            {<<>>, Buffer};
        ContentLengthBin ->
            ContentLength = ?b2i(ContentLengthBin),

            ok = check_max_size(Socket, ContentLength, Opts, Callback),

            case ContentLength - byte_size(Buffer) of
                0 ->
                    {Buffer, <<>>};
                N when N > 0 ->
                    case gen_tcp:recv(Socket, N, body_timeout(Opts)) of
                        {ok, Data} ->
                            {<<Buffer/binary, Data/binary>>, <<>>};
                        {error, closed} ->
                            Mod:handle_event(client_closed, [receiving_body], Args),
                            ok = gen_tcp:close(Socket),
                            exit(normal);
                        {error, timeout} ->
                            Mod:handle_event(client_timeout, [receiving_body], Args),
                            ok = gen_tcp:close(Socket),
                            exit(normal)
                    end;
                _ ->
                    <<Body:ContentLength/binary, Rest/binary>> = Buffer,
                    {Body, Rest}
            end
    end.


ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1).


check_max_size(Socket, ContentLength, Opts, {Mod, Args}) ->
    case ContentLength > max_body_size(Opts) of
        true ->
            Mod:handle_event(bad_request, [{body_size, ContentLength}], Args),
            gen_tcp:close(Socket),
            exit(normal);
        false ->
            ok
    end.


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

content_length(Headers, Body)->
    case proplists:get_value(<<"Content-Length">>, Headers) of
        undefined ->
            {<<"Content-Length">>, iolist_size(Body)};
        ContentLength ->
            {<<"Content-Length">>, ContentLength}
    end.

-spec get_range(RequestHeaders::headers(),
                Size::non_neg_integer()) -> byte_range().

%% @doc: Parses the Range header from the request, if present,
%% as defined in rfc2616-14.35.1. At the moment only
%% single byte-range requests are supported.
get_range(RequestHeaders, Size)  ->
    parse_range(proplists:get_value(<<"Range">>, RequestHeaders), Size).

parse_range(<<$b,$y,$t,$e,$s,$=,$-, SuffixBin/binary>>, Size) ->
    %% make sure the dash is not an invalid negative sign.
    case binary:matches(SuffixBin, <<"-">>) of
	[] ->
	    %% suffix-byte-range
	    SuffixLength = ?b2i(SuffixBin),
	    parse_range({erlang:max(Size - SuffixLength, 0), SuffixLength}, Size);
	_ -> invalid
    end;	    
parse_range(<<$b,$y,$t,$e,$s,$=, ByteRange/binary>>, Size) ->
    case binary:split(ByteRange, <<"-">>, [global]) of
        %% byte-range without last-byte-pos
        [FirstBytePosBin, <<>>] ->
            parse_range({?b2i(FirstBytePosBin), Size}, Size);
        %% full byte-range
        [FirstBytePosBin, LastBytePosBin] ->
            First = ?b2i(FirstBytePosBin),
            Last = ?b2i(LastBytePosBin),
            parse_range({First, Last - First + 1}, Size);
        _ -> invalid
    end;
parse_range({Offset, Length}, Size)
  when Offset < 0; Offset >= Size; Length =< 0; Size =< 0 -> invalid;
parse_range({Offset, Length}, Size)
  when Length > Size - Offset -> {Offset, Size - Offset};
parse_range({Offset, Length}, _) -> {Offset, Length};
parse_range(undefined, _) -> undefined;
parse_range(_, _) -> invalid.

-spec set_content_range(Range::byte_range(), Size::non_neg_integer(),
                        Headers::headers()) -> headers().

set_content_range(undefined, _, Headers) ->
    Headers;
set_content_range(Range, Size, Headers) ->
    lists:keystore(<<"Content-Range">>, 1, Headers,
                   {<<"Content-Range">>,
                    [<<"bytes ">>, set_content_range_bytes(Range),
                     <<"/">>, ?i2l(Size)]}).

set_content_range_bytes({Offset, Length}) ->
    [?i2l(Offset), <<"-">>, ?i2l(Offset + Length - 1)];
set_content_range_bytes(invalid) -> <<"*">>.

-spec set_content_length(Range::byte_range(), Size::non_neg_integer(),
                         Headers::headers()) -> headers().

set_content_length({_, Length}, _, Headers) ->
    lists:keystore(<<"Content-Length">>, 1, Headers,
                   {<<"Content-Length">>, Length});
set_content_length(undefined, Size, Headers) ->
    set_content_length({0, Size}, Size, Headers);
set_content_length(invalid, Size, Headers) ->
    set_content_length({0,0}, Size, Headers).

-spec set_range_and_length(Range::byte_range(), Size::non_neg_integer(),
                           Headers::headers()) -> headers().

set_range_and_length(Range, Size, Headers) ->
    set_content_length(Range, Size, set_content_range(Range, Size, Headers)).


%%
%% PATH HELPERS
%%

parse_path({abs_path, FullPath}) ->
    case binary:split(FullPath, [<<"?">>]) of
        [URL]       -> {ok, {FullPath, split_path(URL), []}};
        [URL, Args] -> {ok, {FullPath, split_path(URL), split_args(Args)}}
    end;
parse_path({absoluteURI, _Scheme, _Host, _Port, Path}) ->
    parse_path({abs_path, Path});
parse_path(_) ->
    {error, unsupported_uri}.

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
%% TIMING HELPERS
%%

%% @doc: Record the current time in the process dictionary. This
%% allows easily adding time tracing wherever, without passing along
%% any variables.
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


%%
%% OPTIONS
%%

accept_timeout(Opts)  -> proplists:get_value(accept_timeout, Opts).
request_timeout(Opts) -> proplists:get_value(request_timeout, Opts).
header_timeout(Opts)  -> proplists:get_value(header_timeout, Opts).
body_timeout(Opts)    -> proplists:get_value(body_timeout, Opts).
max_body_size(Opts)   -> proplists:get_value(max_body_size, Opts).


%%
%% HTTP STATUS CODES
%%

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
