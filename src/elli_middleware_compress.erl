-module(elli_middleware_compress).
-export([postprocess/3]).

postprocess(Req, {ResponseCode, Body}, Config)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    postprocess(Req, {ResponseCode, [], Body}, Config);

postprocess(Req, {ResponseCode, Headers, Body} = Res, Config)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    Threshold = proplists:get_value(compress_byte_size, Config, 1024),
    case should_compress(Body, Threshold) of
        false ->
            Res;
        true ->
            case compress(Body, Req) of
                no_compress ->
                    Res;
                {CompressedBody, Encoding} ->
                    NewHeaders = [{<<"Content-Encoding">>, Encoding} | Headers],
                    {ResponseCode, NewHeaders, CompressedBody}
            end
    end;
postprocess(_, Res, _) ->
    Res.





compress(Body, Req) ->
    case accepted_encoding(Req) of
        <<"gzip">>    = E -> {zlib:gzip(Body), E};
        <<"deflate">> = E -> {zlib:compress(Body), E};
        _                 -> no_compress
    end.



accepted_encoding(Req) ->
    Encodings = binary:split(
                  elli_request:get_header(<<"Accept-Encoding">>, Req, <<>>),
                  [<<",">>, <<";">>], [global]),
    case Encodings of
        [E] -> E;
        [E|_] -> E
    end.

should_compress(Body, S) when byte_size(Body) >= S -> true;
should_compress(_, _)                              -> false.

