-module(elli_middleware_file_compress).

-export([postprocess/3]).

-include_lib("kernel/include/file.hrl").

postprocess(Req, {ResponseCode, Body}, Config) ->
    postprocess(Req, {ResponseCode, [], Body}, Config);

postprocess(Req, {ResponseCode, Headers, {file, File}} = Res, Config)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    Threshold = proplists:get_value(compress_byte_size, Config, 1024),
    case file:read_file_info(File, [{time, posix}]) of
      {ok, #file_info{size=Size, mtime=MTime}}
        when Size >= Threshold ->
          case compress(Req, File, MTime, Config) of
            no_compress ->
              Res;
            {Compressed, GzSize} ->
              H = {<<"Content-Encoding">>, <<"gzip">>},
              {ResponseCode, [H|replace_size(Headers, [], GzSize)], Compressed}
          end;
      _ ->
        Res
    end;
postprocess(_Req, Res, _Config) ->
  Res.

replace_size([], Acc, Size) ->
  [{<<"Content-Length">>, Size}|Acc];
replace_size([{<<"Content-Length">>, _}|Rest], Acc, Size) ->
  [{<<"Content-Length">>, Size}|Rest] ++ Acc;
replace_size([H|Rest], Acc, Size) ->
  replace_size(Rest, [H|Acc], Size).

compress(Req, File, MTime, Config) ->
  CompressCacheDir = proplists:get_value(compress_cache_dir, Config, same),
  case accepted_encoding(Req) of
      <<"gzip">> -> gzip_compress(File, MTime, CompressCacheDir);
      _          -> no_compress
  end.

gzip_compress(File, MTime, CompressCacheDir) ->
  GzFile = case CompressCacheDir of
      same -> File ++ ".gz";
      Dir  ->
        Hash = string_hash(File),
        filename:absname_join(Dir, Hash ++ "-" ++ filename:basename(File) ++ ".gz")
    end,
  case file:read_file_info(GzFile, [{time, posix}]) of
    {ok, #file_info{mtime=GzMtime, size=Size}} when MTime =< GzMtime ->
      {{file, GzFile}, Size};
    _ ->
      gzip_do_compress(File, GzFile)
  end.

gzip_do_compress(File, GzFile) ->
  case file:read_file(File) of
    {ok, Contents} ->
      Zipped = zlib:gzip(Contents),
      ok = file:write_file(GzFile, Zipped, [raw]),
      %io:format(user, "Returning newly zipped data ~p ~p bytes~n", [GzFile, size(Zipped)]),
      %{Zipped, size(Zipped)};
      {{file, GzFile}, size(Zipped)};
    _ ->
      no_compress
  end.

string_hash(String) ->
  integer_to_list(binary:decode_unsigned(crypto:hash(md5, String)), 36).

accepted_encoding(Req) ->
    Encodings = binary:split(
                  elli_request:get_header(<<"Accept-Encoding">>, Req, <<>>),
                  [<<",">>, <<";">>], [global]),
    case Encodings of
        [E] -> E;
        [E|_] -> E
    end.
