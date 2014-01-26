-module(master).
-compile(export_all).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {board, iteration}).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	discover_nodes(),
	c:nl(master),
	c:nl(slave),
	c:nl(board_utils),
	Board = board_utils:create_board(10),
	{ok, #state{board = Board, iteration = 0}}.

iterate(Iterations, State = #state{board = Board, iteration = Iteration}) ->
	Active_nodes = discover_nodes(),
	Slaves = slave:start_slaves(Active_nodes),
	Slave_boards = board_utils:divide(Board, Active_nodes),

	Slave_with_boards = lists:zip(Slaves, Slave_boards),
	order_slaves(Iterations, Slave_with_boards),

	slave:stop_slaves(Slaves),
	{ok, State#state{iteration = Iteration + Iterations}}.


order_slaves(Iterations, [H, T]) ->
	order_slaves(Iterations, none, H, T).

order_slaves(Iterations, none, {C_pid, C_board}, [{N_pid, N_board}|T]) ->
	slave:iterate(C_pid, Iterations, C_board, none, N_pid),
	order_slaves(Iterations, {C_pid, C_board}, {N_pid, N_board}, T);

order_slaves(Iterations, {P_pid, _}, {C_pid, C_board}, []) ->
	slave:iterate(C_pid, Iterations, C_board, P_pid, none);

order_slaves(Iterations, {P_pid, _}, {C_pid, C_board}, [{N_pid, N_board}|T]) ->
	slave:iterate(C_pid, Iterations, C_board, P_pid, N_pid),
	order_slaves(Iterations, {C_pid, C_board}, {N_pid, N_board}, T).


discover_nodes() ->
	lists:foldl(fun count_pongs/2, 0, lists:map(fun net_adm:ping/1,[l@le1, l@le2, l@le3, l@le4, l@le5, l@le6, l@le7, l@le8, l@le9, l@le10])).

count_pongs(pong, Sum) -> Sum + 1;
count_pongs(_, Sum) -> Sum.

%% Client API

next() ->
	gen_server:call({local, ?MODULE}, {next, 1}).

next(N) ->
	gen_server:call({local, ?MODULE}, {next, N}).

%% Public API

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks

handle_call({next, Iterations}, _From, State) ->
	{ok, New_state} = iterate(Iterations, State),
	{reply, New_state#state.board, New_state};

handle_call(stop, _From, State) ->
  say("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.


handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  say("terminate ~p, ~p", [_Reason, _State]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

