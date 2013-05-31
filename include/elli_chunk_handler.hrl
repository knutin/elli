%%% To be included by modules that implement an elli_chunk_handler.
%%% Synopsis:
%%%
%%% -include_lib("elli/include/elli_chunk_handler.hrl").

-behaviour(elli_chunk_handler).
-include_lib("elli/include/elli_handler.hrl").
-export([chunk_loop/1]).
-spec chunk_loop(Ref :: chunk_ref()) -> ok.
