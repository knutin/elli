-module(elli).
-behaviour(gen_server).
-include("elli.hrl").

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {socket, acceptors = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> start_link([]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([_Opts]) ->
    {ok, Socket} = gen_tcp:listen(8080, [binary,
                                         {reuseaddr, true},
                                         {packet, raw}]),
    Acceptors = [start_acceptor(Socket) || _ <- lists:seq(1, 20)],
    {ok, #state{socket = Socket, acceptors = length(Acceptors)}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(accepted, State) ->
    start_acceptor(State#state.socket),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info({'EXIT', _, normal}, State) ->
%%     %%start_acceptor(State#state.socket),
%%     {noreply, State};

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



start_acceptor(Socket) ->
    elli_acceptor:start_link(self(), Socket).
