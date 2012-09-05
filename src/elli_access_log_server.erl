-module(elli_access_log_server).
-behaviour(gen_server).

%% API
-export([start_link/2, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, facility}).

%%
%% API
%%

start_link(Name, Facility) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Facility], []).

log(Name, Msg) ->
    (catch ets:insert(Name, {erlang:now(), Msg})).

%%
%% gen_server callbacks
%%

init([Name, Facility]) ->
    Name = ets:new(Name, [ordered_set, named_table, public,
                              {write_concurrency, true}]),

    erlang:send_after(1000, self(), flush),
    {ok, #state{name = Name, facility = Facility}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State) ->
    erlang:send_after(1000, self(), flush),
    Opts = [{ident, node()},
            {facility, State#state.facility}],
    SyslogMessages = lists:map(fun ({_Ts, Msg}) ->
                                       {Msg, Opts}
                               end, flush(State#state.name)),

    syslog:multi_send(State#state.name, SyslogMessages),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% INTERNAL
%%


flush(Name) ->
    flush_before(os:timestamp(), Name).


flush_before(Now, Name) ->
    flush_before(Now, Name, ets:first(Name), []).

flush_before(_Now, _Name, '$end_of_table', Acc) ->
    Acc;
flush_before(Now, Name, Key, Acc) when Now >= Key ->
    [Value] = ets:lookup(Name, Key),
    true = ets:delete(Name, Key),
    flush_before(Now, Name, ets:next(Name, Key), [Value | Acc]);
flush_before(_Now, _Name, _Key, Acc) ->
    Acc.

