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
            error_response(Req, Res)
    end;
postprocess(_, Res, _) ->
    Res.

%%
%% INTERNALS
%%

is_error(ResponseCode) ->
    ResponseCode >= 400 andalso ResponseCode < 600.

error_response(Req, {ResponseCode, Headers, Body} = Res) ->
    NewBody = <<"<html>"
             "<head><title>", Body/binary, "</title></head>"
             "<body>"
             "<h1>", Body/binary, "</h1>"
             "<p>Request id: #", (list_to_binary(
                                    integer_to_list(
                                      elli_request:id(Req))))/binary, "</p>"
             "</body>"
             "</html>">>,

    {ResponseCode, Headers, NewBody}.