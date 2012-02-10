-module(elli).
-behaviour(gen_server).
-include("elli.hrl").

%% API
-export([start_link/0, start_link/1, get_acceptors/1, get_open_reqs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {socket, acceptors = 0, open_reqs = 0, callback}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> start_link([{callback, fun elli_example:handle_req/1}]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

get_acceptors(S) ->
    gen_server:call(S, get_acceptors).

get_open_reqs(S) ->
    gen_server:call(S, get_open_reqs).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Opts]) ->
    process_flag(trap_exit, true),
    Callback = proplists:get_value(callback, Opts),

    {ok, Socket} = gen_tcp:listen(8080, [binary,
                                         {reuseaddr, true},
                                         {packet, raw}]),
    Acceptors = [start_acceptor(Socket, Callback) || _ <- lists:seq(1, 20)],

    {ok, #state{socket = Socket, acceptors = Acceptors, callback = Callback}}.

handle_call(get_acceptors, _From, State) ->
    {reply, {ok, State#state.acceptors}, State};

handle_call(get_open_reqs, _From, State) ->
    {reply, {ok, State#state.open_reqs}, State}.

handle_cast(accepted, State) ->
    Pid = start_acceptor(State#state.socket, State#state.callback),
    {noreply, State#state{acceptors = [Pid | State#state.acceptors],
                          open_reqs = State#state.open_reqs + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, #state{acceptors = Acceptors} = State) ->
    {noreply, State#state{acceptors = lists:delete(Pid, Acceptors),
                          open_reqs = State#state.open_reqs - 1}};

handle_info(_Info, State) ->
    io:format("elli got ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


start_acceptor(Socket, Callback) ->
    elli_acceptor:start_link(self(), Socket, Callback).
