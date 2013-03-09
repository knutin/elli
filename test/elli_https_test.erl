-module(elli_https_test).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(not_found()),
	  ?_test(crash()),
	  ?_test(no_compress()),
	  ?_test(exception_flow()),
	  ?_test(user_connection()),
	  ?_test(get_args()),
	  ?_test(post_args()),
	  ?_test(shorthand()),
	  ?_test(too_big_body()),
	  ?_test(too_many_headers()),
	  ?_test(way_too_big_body()),
	  ?_test(content_length()),
	  ?_test(chunked()),
	  ?_test(head()),
	  ?_test(no_body())
     ]}.


setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "test"]),
    CertFile = filename:join(CertDir, "server_cert.pem"),
    KeyFile = filename:join(CertDir, "server_key.pem"),
    
    {ok, P} = elli:start_link([
    	{port, {ssl, 3443}},
    	{listen_opts, [
    		{keyfile, KeyFile},
    		{certfile, CertFile}
    	]}, 
    	{callback, elli_example_callback}
    	
    ]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


hello_world() ->
    URL = "https://localhost:3443/hello/world",
    {ok, Response} = httpc:request(get, {URL, []}, [{ssl, [{verify,0}]}], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "12"}], headers(Response)),
    ?assertEqual("Hello World!", body(Response)).

not_found() ->
    {ok, Response} = httpc:request("https://localhost:3443/foobarbaz"),
    ?assertEqual(404, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "9"}], headers(Response)),
    ?assertEqual("Not Found", body(Response)).

crash() ->
    {ok, Response} = httpc:request("https://localhost:3443/crash"),
    ?assertEqual(500, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "21"}], headers(Response)),
    ?assertEqual("Internal server error", body(Response)).


no_compress() ->
    {ok, Response} = httpc:request(get, {"https://localhost:3443/compressed",
                                         [{"Accept-Encoding", "gzip"}]}, [], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 list_to_binary(body(Response))).

exception_flow() ->
    {ok, Response} = httpc:request("https://localhost:3443/403"),
    ?assertEqual(403, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "9"}], headers(Response)),
    ?assertEqual("Forbidden", body(Response)).

user_connection() ->
    {ok, Response} = httpc:request("https://localhost:3443/user/defined/behaviour"),
    ?assertEqual(304, status(Response)),
    ?assertEqual([{"connection", "close"},
                  {"content-length", "123"}], headers(Response)),
    ?assertEqual([], body(Response)).

get_args() ->
    {ok, Response} = httpc:request("https://localhost:3443/hello?name=knut"),
    ?assertEqual("Hello knut", body(Response)).

post_args() ->
    Body = <<"name=foo&baz=quux">>,
    ContentType = "application/x-www-form-urlencoded",

    {ok, Response} = httpc:request(
                       post,
                       {"https://localhost:3443/hello", [], ContentType, Body},
                       [], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual("Hello foo", body(Response)).

shorthand() ->
    {ok, Response} = httpc:request("https://localhost:3443/shorthand"),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "5"}], headers(Response)),
    ?assertEqual("hello", body(Response)).

too_many_headers() ->
    Headers = lists:duplicate(100, {"X-Foo", "Bar"}),
    {ok, Response} = httpc:request(get, {"https://localhost:3443/foo", Headers},
                                   [], []),
    ?assertEqual(400, status(Response)).

too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 1000) + 1),
    {ok, Response} = httpc:request(post,
                                   {"https://localhost:3443/foo", [], [], Body},
                                   [], []),
    ?assertEqual(413, status(Response)).

way_too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 2000) + 1),
    ?assertEqual({error, socket_closed_remotely},
                 httpc:request(post,
                               {"https://localhost:3443/foo", [], [], Body},
                               [], [])).


content_length() ->
    {ok, Response} = httpc:request("https://localhost:3443/304"),

    ?assertEqual(304, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "7"},
                  {"etag", "foobar"}], headers(Response)),
    ?assertEqual([], body(Response)).


chunked() ->
    Expected = "chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1",

    {ok, Response} = httpc:request("https://localhost:3443/chunked"),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  %% httpc adds a content-length, even though elli
                  %% does not send any for chunked transfers
                  {"content-length", integer_to_list(length(Expected))},
                  {"content-type", "text/event-stream"}], headers(Response)),
    ?assertEqual(Expected, body(Response)).

head() ->
    {ok, Response} = httpc:request(head, {"https://localhost:3443/head", []},
                                   [], []),
    ?assertEqual(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "20"}], headers(Response)),
    ?assertEqual([], body(Response)).


no_body() ->
    {ok, Response} = httpc:request("https://localhost:3443/304"),
    ?assertEqual(304, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  {"content-length", "7"},
                  {"etag", "foobar"}], headers(Response)),
    ?assertEqual([], body(Response)).


%%
%% HELPERS
%%

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
	lists:sort(Headers).
