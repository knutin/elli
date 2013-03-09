%%
%% Adapted to provide https support for elli.
%% 
%% Original from mochiweb_socket.erl @copyright 2010 Mochi Media, Inc.
%%

%% @doc elli_tcp - wrapper for plain gen_tcp and ssl sockets.

-module(elli_tcp).

-export([
    listen/2, 
    accept/2, 
    recv/3, 
    send/2,
    sendfile/5,
    close/1, 
    port/1, 
    peername/1, 
    setopts/2, 
    type/1]).


%% @doc listen(Ssl, Port, Opts, SslOpts)
%%
listen({ssl, Port}, Opts) ->
    case ssl:listen(Port, Opts) of
        {ok, ListenSocket} ->
            {ok, {ssl, ListenSocket}};
        {error, _} = Err ->
            Err
    end;
listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).


%%
%%
accept({ssl, ListenSocket}, Timeout) when Timeout > 2 ->
    AcceptTimeout = 1 + erlang:round(Timeout / 10),
    HandshakeTimeout = Timeout - AcceptTimeout,
    % There's a bug in ssl:transport_accept/2 at the moment, which is the
    % reason for the try...catch block. Should be fixed in OTP R14.
    try ssl:transport_accept(ListenSocket, AcceptTimeout) of
        {ok, Socket} ->
            case ssl:ssl_accept(Socket, HandshakeTimeout) of
                ok ->
                    {ok, {ssl, Socket}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason}
    end;
accept(ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout).

%%
recv({ssl, Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

%%
send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

%% 
sendfile(_Fd, {ssl, _Socket}, _Offset, _Length, _Opts) ->
    % TODO: implement a fallback mechanism which works for ssl.
    {error, not_supported};
sendfile(Fd, Socket, Offset, Length, Opts) ->
    file:sendfile(Fd, Socket, Offset, Length, Opts).

%%
close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

%%
port({ssl, Socket}) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        {error, _} = Err ->
            Err
    end;
port(Socket) ->
    inet:port(Socket).

%%
peername({ssl, Socket}) ->
    ssl:peername(Socket);
peername(Socket) ->
    inet:peername(Socket).


%%
setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

%%
type({ssl, _}) ->
    ssl;
type(_) ->
    plain.

