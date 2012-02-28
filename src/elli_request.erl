-module(elli_request).
-include("elli.hrl").

-export([handle/2, send_chunk/2, chunk_ref/1, path/1]).


-spec handle(#req{}, callback()) -> {response(), connection_token_atom() | chunked}.
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
                       content_length(Body),
                       content_encoding(Encoding)
                       | UserHeaders],

            Response = [responsecode2bin(ResponseCode), <<"\r\n">>,
                        encode_headers(Headers), <<"\r\n">>,
                        Body],

            {Response, close_or_keepalive(Req, UserHeaders)}
    end.


-spec execute_callback(#req{}, callback()) ->
                              {response_code(), headers(), body()} |
                              {chunk, headers()}.
execute_callback(Req, {CallbackMod, CallbackArgs}) ->
    try CallbackMod:handle(Req, CallbackArgs) of
        {ok, Headers, Body}       -> {200, Headers, Body};
        {HttpCode, Headers, Body} -> {HttpCode, Headers, Body};
        {chunk, Headers}          -> {chunk, Headers}
    catch
        throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
            {ResponseCode, Headers, Body};
        throw:Exception ->
            CallbackMod:request_throw(Req, Exception, erlang:get_stacktrace(), CallbackArgs),
            {500, [], <<"Internal server error">>};
        error:Error ->
            CallbackMod:request_error(Req, Error, erlang:get_stacktrace(), CallbackArgs),
            {500, [], <<"Internal server error">>};
        exit:Exit ->
            CallbackMod:request_exit(Req, Exit, erlang:get_stacktrace(), CallbackArgs),
            {500, [], <<"Internal server error">>}
    end.



responsecode2bin(200) -> <<"HTTP/1.1 200 OK">>;
responsecode2bin(304) -> <<"HTTP/1.1 304 Not Modified">>;
responsecode2bin(403) -> <<"HTTP/1.1 403 Forbidden">>;
responsecode2bin(404) -> <<"HTTP/1.1 404 Not Found">>;
responsecode2bin(500) -> <<"HTTP/1.1 500 Internal Server Error">>.



%%
%% Helpers for working with a #req{}
%%


%% @doc: Splits the request path into binary parts useful for
%% matching. TODO: Should this be done once upfront and cached inside
%% the #req{}?
path(#req{path = <<"/", Path/binary>>}) ->
    binary:split(Path, [<<"/">>], [global, trim]);
path(#req{path = Path}) ->
    binary:split(Path, [<<"/">>], [global, trim]).


%% @doc: Returns a reference that can be used to send chunks to the
%% client. If the protocol does not support it, returns {error,
%% not_supported}.
chunk_ref(#req{version = {1, 1}} = Req) ->
    Req#req.pid;
chunk_ref(_) ->
    {error, not_supported}.


send_chunk(Ref, Data) ->
    Ref ! {chunk, Data, self()},
    receive
        {Ref, ok} ->
            ok;
        {Ref, {error, Reason}} ->
            {error, Reason}
    end.





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

close_or_keepalive(Req, _UserHeaders) ->
    case connection_token(Req) of
        <<"Keep-Alive">> -> keep_alive;
        <<"close">>      -> close
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
                Other ->
                    error_logger:info_msg("Other: ~p~n", [Other]),
                    {Body, none}

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
encode_value(V) when is_binary(V)  -> V.

