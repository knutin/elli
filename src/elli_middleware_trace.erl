%% @doc: Helper for testing midldewares. Stores all events in a public
%% ETS table.

-module(elli_middleware_trace).
-export([handle/2, handle_event/3]).
-export([clear/1]).

handle(_, _) ->
    ignore.

handle_event(elli_startup, _, Args) ->
    Name = name(Args),
    Name = ets:new(Name, [named_table, public, ordered_set]),
    ok;

handle_event(Event, Details, Args) ->
    ets:insert(name(Args), {erlang:now(), Event, Details}),
    ok.

clear(Name) ->
    true = ets:delete_all_objects(Name).

name(Args) -> proplists:get_value(name, Args).
