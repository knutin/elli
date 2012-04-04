-module(elli_request).
-include("elli.hrl").

-export([send_chunk/2, chunk_ref/1, path/1, raw_path/1, get_header/2,
         get_arg/2, get_arg/3, body_qs/1, headers/1, peer/1, method/1, body/1,
         get_header/3]).

%%
%% Helpers for working with a #req{}
%%

%% @doc: Returns path split into binary parts.
path(#req{path = Path})                     -> Path.
raw_path(#req{raw_path = {abs_path, Path}}) -> Path.
headers(#req{headers = Headers})            -> Headers.
peer(#req{peer = Peer})                     -> Peer.
method(#req{method = Method})               -> Method.
body(#req{body = Body})                     -> Body.

get_header(Key, #req{headers = Headers}) ->
    proplists:get_value(Key, Headers).

get_header(Key, #req{headers = Headers}, Default) ->
    proplists:get_value(Key, Headers, Default).

get_arg(Key, #req{} = Req) ->
    get_arg(Key, Req, undefined).

get_arg(_Key, #req{args = []}, Default) ->
    Default;
get_arg(Key, #req{args = Args}, Default) ->
    proplists:get_value(Key, Args, Default).

body_qs(#req{body = Body}) ->
    try
        elli_http:split_args(Body)
    catch
        _ -> []
    end.

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
