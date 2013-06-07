%%% To be included by modules that implement an elli_handler.
%%% Synopsis:
%%%
%%% -include_lib("elli/include/elli_handler.hrl").

-behaviour(elli_handler).
-include_lib("elli/include/elli.hrl").
-export([handle/2, handle_event/3]).
-spec handle(Req :: #req{}, callback_args()) -> handler_ret().
-spec handle_event(Event :: elli_event(), Args :: [tuple()], Config :: [tuple()]) -> ok.
