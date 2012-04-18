-module(elli_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(not_found()),
      ?_test(crash()),
      %% ?_test(no_compress()),
      ?_test(exception_flow()),
      ?_test(user_connection()),
      ?_test(get_args()),
      ?_test(shorthand())
%%      ?_test(content_length())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    lhttpc:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback}]),
    unlink(P),
    register(elli, P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

hello_world() ->
    URL = "http://localhost:8080/hello/world",
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "12"}], headers(Response)),
    ?assertEqual(<<"Hello World!">>, body(Response)).



not_found() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/foobarbaz", "GET", [], 1000),
    ?assertEqual({404, "Not Found"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "9"}], headers(Response)),
    ?assertEqual(<<"Not Found">>, body(Response)).

crash() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/crash", "GET", [], 1000),
    ?assertEqual({500, "Internal Server Error"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "21"}], headers(Response)),
    ?assertEqual(<<"Internal server error">>, body(Response)).


%% no_compress() ->
%%     {ok, Response} = lhttpc:request("http://localhost:8080/compressed", "GET",
%%                                     [{"Accept-Encoding", "gzip"}], 1000),
%%     ?assertEqual({200, "OK"}, status(Response)),
%%     ?assertEqual([{"Connection", "Keep-Alive"},
%%                   {"Content-Length", "1032"}], headers(Response)),
%%     ?assertEqual(binary:copy(<<"Hello World!">>, 86), body(Response)).

exception_flow() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/403", "GET", [], 1000),
    ?assertEqual({403, "Forbidden"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "9"}], headers(Response)),
    ?assertEqual(<<"Forbidden">>, body(Response)).

user_connection() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/close", "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "close"},
                  {"Content-Length", "7"}], headers(Response)),
    ?assertEqual(<<"closing">>, body(Response)).


get_args() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/hello?name=knut",
                                    "GET", [], 1000),
    ?assertEqual(<<"Hello knut">>, body(Response)).

shorthand() ->
    {ok, Response} = lhttpc:request("http://localhost:8080/shorthand",
                                    "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual([{"Connection", "Keep-Alive"},
                  {"Content-Length", "5"}], headers(Response)),
    ?assertEqual(<<"hello">>, body(Response)).



%% content_length() ->
%%     S = s(),
%%     {ok, Response} = lhttpc:request("http://localhost:8080/304",
%%                                     "GET", [], 1000),

%%     ?assertEqual({304, "OK"}, status(Response)),
%%     ?assertEqual([{"Connection", "Keep-Alive"},
%%                   {"Content-Length", "0"}], headers(Response)),
%%     ?assertEqual(<<>>, body(Response)),

%%     stop(S).


body_qs_test() ->
    ?assertEqual([{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"bang">>}, {<<"found">>, true}],
                 elli_request:body_qs(#req{body = <<"foo=bar&baz=bang&found">>})).

to_proplist_test() ->
    Req = #req{method = 'GET',
               path = [<<"crash">>],
               args = [],
               version = {1,1},
               raw_path = <<"/crash">>,
               headers = [{<<"Host">>,<<"localhost:8080">>}],
               body = <<>>,
               pid = self(),
               peer = <<"127.0.0.1">>},

    Prop = [{method,'GET'},
            {path,[<<"crash">>]},
            {args,[]},
            {raw_path,<<"/crash">>},
            {version,{1,1}},
            {headers,[{<<"Host">>,<<"localhost:8080">>}]},
            {body,<<>>},
            {pid,self()},
            {peer,<<"127.0.0.1">>}],
    ?assertEqual(Prop, elli_request:to_proplist(Req)).


query_str_test_() ->
    MakeReq = fun(Path) -> #req{raw_path = Path} end,
    [
        % For empty query strings, expect `query_str` to return an empty binary.
        ?_assertEqual(<<>>, elli_request:query_str(MakeReq(<<"/foo">>))),
        ?_assertEqual(<<>>, elli_request:query_str(MakeReq(<<"/foo?">>))),
        % Otherwise it should return everything to the right hand side of `?`.
        ?_assertEqual(<<"bar=baz&baz=bang">>,
                      elli_request:query_str(MakeReq(<<"/foo?bar=baz&baz=bang">>)))
    ].


register_test() ->
    ?assertEqual(undefined, whereis(elli)),
    {ok, Pid} = elli:start_link([{name, {local, elli}}, {callback, elli_example_callback}]),
    ?assertEqual(Pid, whereis(elli)),
    ok.


%%
%% HELPERS
%%

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).
