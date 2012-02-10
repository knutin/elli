-module(elli_acceptor).
-include("elli.hrl").

-export([start_link/3, accept/3, handle_request/2]).

start_link(Server, ListenSocket, Callback) ->
    proc_lib:spawn_link(?MODULE, accept, [Server, ListenSocket, Callback]).


accept(Server, ListenSocket, Callback) ->
    %% TODO: timeout, call ?MODULE:init again
    case catch gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ?MODULE:handle_request(Socket, Callback),
            exit(normal);
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket, Callback)
    end.

handle_request(Socket, Callback) ->
    inet:setopts(Socket, [{packet, http_bin}, {active, once}]),
    receive
        {http, _, {http_request, Method, {abs_path, Path}, Version}} ->

            case elli_request:handle(Socket, Callback, Method, Path, Version) of
                {Response, <<"Keep-Alive">>} ->
                    ok = gen_tcp:send(Socket, Response),
                    ?MODULE:handle_request(Socket, Callback);
                {Response, <<"close">>} ->
                    ok = gen_tcp:send(Socket, Response),
                    gen_tcp:close(Socket),
                    exit(closing)
            end
    after 1000 ->
            io:format("nothing sent by client, exiting~n"),
            gen_tcp:close(Socket),
            exit(client_timeout)
    end.

