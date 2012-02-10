-module(elli_example).
-compile([export_all]).

handle_req(_Req) ->
    {ok, [], <<"Hello World!">>}.
