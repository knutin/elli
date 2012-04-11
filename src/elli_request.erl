-module(elli_request).
-include("elli.hrl").

-export([send_chunk/2
         , chunk_ref/1
         , path/1
         , raw_path/1
         , query_str/1
         , get_header/2
         , get_arg/2
         , get_arg/3
         , body_qs/1
         , headers/1
         , peer/1
         , method/1
         , body/1
         , get_header/3
         , to_proplist/1
        ]).

%%
%% Helpers for working with a #req{}
%%

-spec query_str(#req{}) -> QueryStr :: binary().
%% @doc Returns the query string associated with the given Request as a binary.
%%      #req.query_str is populated in elli_http:parse_path/1.
query_str(#req{query_str = Qs}) -> Qs.


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

%% @doc Parses application/x-www-form-urlencoded body into a proplist
body_qs(#req{body = Body}) ->
    elli_http:split_args(Body).


to_proplist(#req{} = Req) ->
    lists:zip(record_info(fields, req), tl(tuple_to_list(Req))).



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
