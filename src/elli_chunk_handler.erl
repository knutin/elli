%%% Defines elli_chunk_handler behaviour.
-module(elli_chunk_handler).
-include("elli.hrl").

-callback chunk_loop(Ref :: chunk_ref()) -> ok.

%% EOF elli_chunk_handler.erl
