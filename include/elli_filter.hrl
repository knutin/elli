%%% To be included by modules that implement an elli_filter,
%%% that is a pre-/postprocessor called by elli_middleware.
%%% Synopsis:
%%%
%%% -include_lib("elli/include/elli_filter.hrl").

-behaviour(elli_filter).
-include_lib("elli/include/elli.hrl").
-export([preprocess/2, postprocess/3]).

-spec preprocess(Req :: #req{}, Config :: cfg()) ->  #req{}.
-spec postprocess(Req :: #req{}, tuple(), Config :: cfg()) -> tuple().
