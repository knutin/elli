-module(elli_example_callback).
-export([handle/2, handle_event/3]).
-export([chunk_loop/1]).

-include("elli.hrl").
-behaviour(elli_handler).


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

handle('GET', [<<"compressed">>], _Req) ->
    %% Body with a byte size over 1024 are automatically gzipped
    {ok, [], binary:copy(<<"Hello World!">>, 86)};

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
    {chunk, []};

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
    timer:sleep(1000),

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
