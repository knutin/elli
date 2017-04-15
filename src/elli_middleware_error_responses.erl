%% @doc: Add request reference to error response body as Elli middleware.
%% Postprocesses all requests and add request's unique reference to response
%% body.

-module(elli_middleware_error_responses).
-export([postprocess/3]).

%%
%% Postprocess handler
%%

postprocess(Req, {ResponseCode, Headers, Body} = Res, _Args)
  when is_integer(ResponseCode) ->
    case is_error(ResponseCode) of
        false ->
            Res;
        true ->
            Msg = io_lib:format("~nRequest: ~p", [elli_request:to_proplist(Req)]),
            {ResponseCode, Headers, [Body, Msg]}
    end;
postprocess(_, Res, _) ->
    Res.

%%
%% INTERNALS
%%

is_error(ResponseCode) ->
    ResponseCode >= 400 andalso ResponseCode < 600.