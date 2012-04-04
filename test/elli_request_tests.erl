-module(elli_request_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

body_qs_test() ->
    Expected = [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"bang">>}],
    Actual     = elli_request:body_qs(#req{body = <<"foo=bar&baz=bang">>}),
    ?assertEqual(Expected, Actual).

body_qs2_test() ->
    ?assertEqual([], elli_request:body_qs(#req{body = <<>>})).
