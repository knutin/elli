-module(elli_request).
-include("elli.hrl").

-export([send_chunk/2, chunk_ref/1, path/1, get_header/2]).

%%
%% Helpers for working with a #req{}
%%

%% @doc: Returns path split into binary parts.
path(#req{path = Path}) ->
    Path.

get_header(Key, #req{headers = Headers}) ->
    proplists:get_value(Key, Headers).


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
