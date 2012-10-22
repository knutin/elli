%% @doc: Elli acceptor manager
%%
%% This gen_server owns the listen socket and manages the processes
%% accepting on that socket. When a process waiting for accept gets a
%% request, it notifies this gen_server so we can start up another
%% acceptor.
%%
-module(elli).
-behaviour(gen_server).
-include("../include/elli.hrl").

%% API
-export([start_link/0
         , start_link/1
         , stop/1
         , get_acceptors/1
         , get_open_reqs/1
         , get_open_reqs/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() -> start_link(?EXAMPLE_CONF).

start_link(Opts) ->
    %% Validate options
    Callback = required_opt(callback, Opts),
    valid_callback(Callback) orelse throw(invalid_callback),

    case proplists:get_value(name, Opts) of
        undefined ->
            gen_server:start_link(?MODULE, [Opts], []);
        Name ->
            gen_server:start_link(Name, ?MODULE, [Opts], [])
    end.

get_acceptors(S) ->
    gen_server:call(S, get_acceptors).

get_open_reqs(S) ->
    get_open_reqs(S, 5000).

get_open_reqs(S, Timeout) ->
    gen_server:call(S, get_open_reqs, Timeout).

stop(S) ->
    gen_server:call(S, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Opts]) ->
    %% Use the exit signal from the acceptor processes to know when
    %% they exit
    process_flag(trap_exit, true),

    Callback       = required_opt(callback, Opts),
    CallbackArgs   = proplists:get_value(callback_args, Opts),
    IPAddress      = proplists:get_value(ip, Opts, {0,0,0,0}),
    Port           = proplists:get_value(port, Opts, 8080),
    MinAcceptors   = proplists:get_value(min_acceptors, Opts, 20),

    AcceptTimeout  = proplists:get_value(accept_timeout, Opts, 10000),
    RequestTimeout = proplists:get_value(request_timeout, Opts, 60000),
    HeaderTimeout  = proplists:get_value(header_timeout, Opts, 10000),
    BodyTimeout    = proplists:get_value(body_timeout, Opts, 30000),
    MaxBodySize    = proplists:get_value(max_body_size, Opts, 1024000),

    Options = [{accept_timeout, AcceptTimeout},
               {request_timeout, RequestTimeout},
               {header_timeout, HeaderTimeout},
               {body_timeout, BodyTimeout},
               {max_body_size, MaxBodySize}],

    %% Notify the handler that we are about to start accepting
    %% requests, so it can create necessary supporting processes, ETS
    %% tables, etc.
    ok = Callback:handle_event(elli_startup, [], CallbackArgs),

    {ok, Socket} = gen_tcp:listen(Port, [binary,
                                         {ip, IPAddress},
                                         {reuseaddr, true},
                                         {backlog, 32768},
                                         {packet, raw},
                                         {active, false}
                                        ]),
    Acceptors = [elli_http:start_link(self(), Socket, Options,
                                      {Callback, CallbackArgs})
                 || _ <- lists:seq(1, MinAcceptors)],

    {ok, #state{socket = Socket,
                acceptors = Acceptors,
                open_reqs = 0,
                options = Options,
                callback = {Callback, CallbackArgs}}}.


handle_call(get_acceptors, _From, State) ->
    {reply, {ok, State#state.acceptors}, State};

handle_call(get_open_reqs, _From, State) ->
    {reply, {ok, State#state.open_reqs}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(accepted, State) ->
    {noreply, start_add_acceptor(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    error_logger:error_msg("No more file descriptors, shutting down~n"),
    {stop, emfile, State};

handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:error_msg("Elli request (pid ~p) unexpectedly "
                           "crashed:~n~p~n", [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_acceptor(State, Pid) ->
    State#state{acceptors = lists:delete(Pid, State#state.acceptors),
                open_reqs = State#state.open_reqs - 1}.

start_add_acceptor(State) ->
    Pid = elli_http:start_link(self(), State#state.socket,
                               State#state.options, State#state.callback),
    State#state{acceptors = [Pid | State#state.acceptors],
                open_reqs = State#state.open_reqs + 1}.


required_opt(Name, Opts) ->
    case proplists:get_value(Name, Opts) of
        undefined ->
            throw(badarg);
        Value ->
            Value
    end.


valid_callback(Mod) ->
    lists:member({handle, 2}, Mod:module_info(exports)) andalso
        lists:member({handle_event, 3}, Mod:module_info(exports)).
