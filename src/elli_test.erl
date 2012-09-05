%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @doc
%%%
%%% @end

-module(elli_test).

-include("../include/elli.hrl").

-export([call/5]).

call(Method, RawPath, RequestHeaders, RequestBody, Opts) ->
    Callback = proplists:get_value(callback, Opts),
    CallbackArgs = proplists:get_value(callback_args, Opts),
    Req = elli_http:mk_req(Method, {abs_path, RawPath}, RequestHeaders,
			   RequestBody, {1,1}, undefined),
    ok = Callback:handle_event(elli_startup, [], CallbackArgs),
    Callback:handle(Req, CallbackArgs).
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hello_world_test() ->
    ?assertEqual({ok, [], <<"Hello World!">>},
		 call('GET', <<"/hello/world/">>, [], <<>>,
		      ?example_conf)).

-endif. %% TEST
