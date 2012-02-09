-module(elli_acceptor).
-include("elli.hrl").

-export([start_link/2, accept/2, loop/1]).

start_link(Server, ListenSocket) ->
    proc_lib:spawn_link(?MODULE, accept, [Server, ListenSocket]).


accept(Server, ListenSocket) ->
    %% TODO: timeout, call ?MODULE:init again
    case catch gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ?MODULE:loop(Socket),
            exit(normal)
    end.

loop(Socket) ->
    case elli_request:handle(Socket) of
        keep_alive ->
            loop(Socket);
        close ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

