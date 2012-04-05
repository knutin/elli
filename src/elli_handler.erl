-module(elli_handler).
-include("elli.hrl").

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle, 2},
     {handle_event, 3}].
