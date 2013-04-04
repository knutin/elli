-module(helper_events_server).
-export([start/0, stop/0, add/1, get/0, init/0]).

start() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    ok.

stop() ->
    ?MODULE ! stop,
    unregister(?MODULE).

add(Event) ->
    ?MODULE ! {add, Event},
    ok.

get() ->
    ?MODULE ! {events, self()},
    receive
        X -> X
    end.

init() ->
    loop([]).

loop(Events) ->
    receive
        stop ->
            ok;
        {events, From} ->
            From ! Events,
            loop(Events);
        {add, Event} ->
            loop(lists:append([Event], Events))
    end.