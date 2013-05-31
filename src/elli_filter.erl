%%% @doc Defines elli_filter behaviour. A module that implements 
%%% elli_filter behaviour may be called by the elli_middleware
%%% handler as a pre-/postprocessor to handle requests.
-module(elli_filter).
-include("elli.hrl").

-callback preprocess(Req :: #req{}, Config :: cfg()) ->  #req{}.
-callback postprocess(Req :: #req{}, tuple(), Config :: cfg()) -> tuple().

%% EOF elli_filter.hrl
