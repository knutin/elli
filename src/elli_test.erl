%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @doc
%%%
%%% @end

-module(elli_test).

-include("../include/elli.hrl").

-export([call/5]).

call(Method, Path, Headers, Body, Opts) ->
    Callback = proplists:get_value(callback, Opts),
    CallbackArgs = proplists:get_value(callback_args, Opts),
    Req = elli_http:mk_req(Method, {abs_path, Path}, Headers,
			   Body, {1,1}, undefined),
    ok = Callback:handle_event(elli_startup, [], CallbackArgs),
    Callback:handle(Req, CallbackArgs).
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    ?assertEqual({ok, [], <<"Hello World!">>},
		 elli_test:call('GET', <<"/hello/world/">>, [], <<>>,
				?example_conf)),
    ?assertEqual({ok, [], <<"Hello Test1">>},
		 elli_test:call('GET', <<"/hello/?name=Test1">>, [], <<>>,
				?example_conf)),
    ?assertEqual({ok,
		  [{<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}],
		  <<"{\"name\" : \"Test2\"}">>},
		 elli_test:call('GET', <<"/type?name=Test2">>,
				[{<<"Accept">>, <<"application/json">>}], <<>>,
				?example_conf)).

-endif. %% TEST
