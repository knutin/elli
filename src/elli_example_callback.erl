%% @doc: Elli example callback
%%
%% Your callback needs to implement two functions, handle/2 and
%% handle_event/3. For every request, Elli will call your handle
%% function with the request. When an event happens, like Elli
%% completed a request, there was a parsing error or your handler
%% threw an error, handle_event/3 is called.

-module(elli_example_callback).
-export([handle/2, handle_event/3]).
-export([chunk_loop/1]).

-include("../include/elli.hrl").
-behaviour(elli_handler).

-include_lib("kernel/include/file.hrl").

%%
%% ELLI REQUEST CALLBACK
%%

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).



%% Route METHOD & PATH to the appropriate clause
handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};

handle('GET', [<<"hello">>], Req) ->
    %% Fetch a get argument from the URL.
    Name = elli_request:get_arg(<<"name">>, Req),
    {ok, [], <<"Hello ", Name/binary>>};

handle('GET', [<<"hello">>, <<"iolist">>], Req) ->
    %% Fetch a get argument from the URL.
    Name = elli_request:get_arg(<<"name">>, Req),
    {ok, [], [<<"Hello ">>, Name]};

handle('GET', [<<"type">>], Req) ->
    %% Fetch a get argument from the URL.
    Name = elli_request:get_arg(<<"name">>, Req),
    %% Fetch a header.
    case elli_request:get_header(<<"Accept">>, Req, <<"text/plain">>) of
	<<"text/plain">> ->
	    {ok, [{<<"Content-type">>, <<"text/plain; charset=ISO-8859-1">>}],
	     <<"name: ", Name/binary>>};
	<<"application/json">> ->
	    {ok, [{<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}],
	     <<"{\"name\" : \"", Name/binary, "\"}">>}
    end;

handle('GET',[<<"headers.html">>], _Req) ->
    %% Set custom headers, for example 'Content-Type'
    {ok, [{<<"X-Custom">>, <<"foobar">>}], <<"see headers">>};

handle('GET',[<<"close">>], _Req) ->
    %% Set a custom connection header. Elli will by default use
    %% keep-alive if the browser supports it, this will override that
    %% behaviour.
    {ok, [{<<"Connection">>, <<"close">>}], <<"closing">>};

handle('GET', [<<"crash">>], _Req) ->
    %% Throwing an exception results in a 500 response and
    %% request_throw being called
    throw(foobar);

handle('GET', [<<"sendfile">>], _Req) ->
    %% Returning {file, "/path/to/file"} instead of the body results
    %% in Elli using sendfile. In the name of performance, Elli
    %% requires you to specify the Content-Length header, since you
    %% probably already stated the file to check if it exists.

    F = "../src/elli_example_callback.erl",
    {ok, #file_info{size = Size}} = file:read_file_info(F),

    {200, [{<<"Content-Length">>, Size}], {file, F}};

handle('GET', [<<"compressed">>], _Req) ->
    %% Body with a byte size over 1024 are automatically gzipped by
    %% elli_middleware_compress
    {ok, binary:copy(<<"Hello World!">>, 86)};

handle('GET', [<<"chunked">>], Req) ->
    %% Start a chunked response for streaming real-time events to the
    %% browser.
    %%
    %% Calling elli_request:send_chunk(ChunkRef, Body) will send that
    %% chunk to the client. An empty body will close the
    %% connection. See chunk_loop/1 below.
    %%
    %% Return immediately {chunk, Headers} to signal we want to chunk.
    Ref = elli_request:chunk_ref(Req),
    spawn(fun() -> ?MODULE:chunk_loop(Ref) end),
    {chunk, [{<<"Content-Type">>, <<"text/event-stream">>}]};

handle('GET', [<<"shorthand">>], _Req) ->
    {200, <<"hello">>};

handle('GET', [<<"304">>], _Req) ->
    {304, [], <<>>};

handle('GET', [<<"302">>], _Req) ->
    {302, [{<<"Location">>, <<"/hello/world">>}], <<>>};

handle('GET', [<<"403">>], _Req) ->
    %% Exceptions formatted as return codes can be used to
    %% short-circuit a response, for example in case of
    %% authentication/authorization
    throw({403, [], <<"Forbidden">>});

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.



%% Send 10 separate chunks to the client.
chunk_loop(Ref) ->
    chunk_loop(Ref, 10).

chunk_loop(Ref, 0) ->
    %% Send empty chunk to make elli close the connection
    elli_request:send_chunk(Ref, <<>>);
chunk_loop(Ref, N) ->
    timer:sleep(10),

    %% Send a chunk to the client, check for errors as the user might
    %% have disconnected
    case elli_request:send_chunk(Ref, [<<"chunk">>, integer_to_list(N)]) of
        ok -> ok;
        {error, Reason} ->
            io:format("error in sending chunk: ~p~n", [Reason])
    end,

    chunk_loop(Ref, N-1).


%%
%% ELLI EVENT CALLBACKS
%%

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
