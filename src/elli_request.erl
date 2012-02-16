-module(elli_request).
-include("elli.hrl").

-export([handle/2, send_chunk/2, chunk_ref/1, split_path/1]).


%% @doc: Execute the callback module, create HTTP response based on
%% the result.
handle(Req, Callback) ->
    case execute_callback(Req, Callback) of
        {chunk, UserHeaders} ->
            Headers = [{<<"Transfer-Encoding">>, <<"chunked">>},
                       {<<"Connection">>, connection_token(Req)}
                       | UserHeaders],
            Response = [responsecode2bin(200), <<"\r\n">>,
                        encode_headers(Headers), <<"\r\n">>],
            {Response, chunked};

        {ResponseCode, UserHeaders, UserBody} ->
            {Body, Encoding} = encode_body(UserBody, Req),
            Headers = [
                       {<<"Connection">>, connection_token(Req)},
                       {<<"Content-Length">>, size(Body)},
                       content_encoding(Encoding)
                       | UserHeaders],

            Response = [responsecode2bin(ResponseCode), <<"\r\n">>,
                        encode_headers(Headers), <<"\r\n">>,
                        Body],

            {Response, connection_atom(Req)}
    end.



execute_callback(Req, Callback) ->
    try Callback:handle(Req) of
        {ok, Headers, Body}       -> {200, Headers, Body};
        {HttpCode, Headers, Body} -> {HttpCode, Headers, Body};
        {chunk, Headers}          -> {chunk, Headers}
    catch
        throw:Exception ->
            Callback:request_throw(Req, Exception),
            {500, [], <<>>};
        exit:Exit ->
            Callback:request_exit(Req, Exit),
            {500, [], <<>>}
    end.


chunk_ref(Req) ->
    Req#req.pid.

send_chunk(Ref, Data) ->
    Ref ! {chunk, Data, self()},
    receive
        {Ref, ok} ->
            ok;
        {Ref, {error, Reason}} ->
            {error, Reason}
    end.



responsecode2bin(200) -> <<"HTTP/1.1 200 OK">>;
responsecode2bin(404) -> <<"HTTP/1.1 404 Not Found">>;
responsecode2bin(500) -> <<"HTTP/1.1 500 Internal Server Error">>.


split_path(#req{path = <<"/", Path/binary>>}) ->
    binary:split(Path, [<<"/">>], [global]);

split_path(#req{path = Path}) ->
    binary:split(Path, [<<"/">>], [global]).





connection_token(#req{version = {1, 1}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"close">> -> <<"close">>;
        _           -> <<"Keep-Alive">>
    end;

connection_token(#req{version = {1, 0}, headers = Headers}) ->
    case proplists:get_value(<<"Connection">>, Headers) of
        <<"Keep-Alive">> -> <<"Keep-Alive">>;
        _                -> <<"close">>
    end.

connection_atom(Req) ->
    case connection_token(Req) of
        <<"Keep-Alive">> -> keep_alive;
        <<"close">>      -> close
    end.

content_encoding(gzip)    -> {<<"Content-Encoding">>, <<"gzip">>};
content_encoding(deflate) -> {<<"Content-Encoding">>, <<"deflate">>};
content_encoding(none)    -> [].


accepted_encoding(Headers) ->
    Encodings = binary:split(
                  proplists:get_value(<<"Accept-Encoding">>, Headers, <<>>),
                  [<<",">>], [global]),
    case Encodings of
        [E] -> E;
        [E|_] -> E
    end.

encode_body(Body, #req{headers = Headers}) ->
    case should_compress(Body) of
        true ->
            case accepted_encoding(Headers) of
                <<"gzip">>    -> {zlib:gzip(Body), gzip};
                <<"deflate">> -> {zlib:compress(Body), deflate}
            end;
        false ->
            {Body, none}
    end.


should_compress(Body) when size(Body) >= 1024 -> true;
should_compress(_)                            -> false.


encode_headers([]) ->
    [];

encode_headers([[] | H]) ->
    encode_headers(H);
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> ?i2l(V);
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_atom(V)    -> atom_to_binary(V, latin1).
