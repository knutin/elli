-module(elli_middleware).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).


handle(Req, Config) ->
    forward_request(Req, mods(Config)).

handle_event(Event, Args, Config) ->
    forward_event(Event, Args, mods(Config)).



%%
%% MIDDLEWARE LOGIC
%%

forward_request(_Req, []) ->
    {404, [], <<"Not Found">>};
forward_request(Req, [{Mod, Args} | Mods]) ->
    case Mod:handle(Req, Args) of
        ignore ->
            forward_request(Req, Mods);
        {forward, NewReq} ->
            forward_request(NewReq, Mods);
        Response ->
            Response
    end.

forward_event(F, A, Mods) ->
    lists:map(fun ({M, ExtraArgs}) ->
                      M:handle_event(F, A, ExtraArgs)
              end, Mods),
    ok.

%%
%% INTERNAL HELPERS
%%

mods(Config) -> proplists:get_value(mods, Config).
