-module(elli_middleware).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).


handle(CleanReq, Config) ->
    Mods = mods(Config),
    PreReq = preprocess(CleanReq, Mods),
    Res    = process(PreReq, Mods),
    postprocess(PreReq, Res, lists:reverse(Mods)).



handle_event(elli_startup, Args, Config) ->
    lists:foreach(fun code:ensure_loaded/1, lists:map(fun ({M, _}) -> M end,
                                                      mods(Config))),
    forward_event(elli_startup, Args, mods(Config));
handle_event(Event, Args, Config) ->
    forward_event(Event, Args, Config).




%%
%% MIDDLEWARE LOGIC
%%

process(_Req, []) ->
    {404, [], <<"Not Found">>};
process(Req, [{Mod, Args} | Mods]) ->
    case erlang:function_exported(Mod, handle, 2) of
        true ->
            case Mod:handle(Req, Args) of
                ignore ->
                    process(Req, Mods);
                Response ->
                    Response
            end;
        false ->
            process(Req, Mods)
    end.

preprocess(Req, []) ->
    Req;
preprocess(Req, [{Mod, Args} | Mods]) ->
    case erlang:function_exported(Mod, preprocess, 2) of
        true ->
            preprocess(Mod:preprocess(Req, Args), Mods);
        false ->
            preprocess(Req, Mods)
    end.

postprocess(_Req, Res, []) ->
    Res;
postprocess(Req, Res, [{Mod, Args} | Mods]) ->
    case erlang:function_exported(Mod, postprocess, 3) of
        true ->
            postprocess(Req, Mod:postprocess(Req, Res, Args), Mods);
        false ->
            postprocess(Req, Res, Mods)
    end.


forward_event(Event, Args, Mods) ->
    lists:foreach(
      fun ({M, ExtraArgs}) ->
              case erlang:function_exported(M, handle_event, 3) of
                  true ->
                      M:handle_event(Event, Args, ExtraArgs);
                  false ->
                      ok
              end
      end, Mods),
    ok.


%%
%% INTERNAL HELPERS
%%

mods(Config) -> proplists:get_value(mods, Config).
