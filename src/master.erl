-module(master).
-compile(export_all).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {board, iteration, slaves, slaves_with_boards, response_count, caller_pid}).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	%discover_nodes(),
	c:nl(master),
	c:nl(slave),
	c:nl(board_utils),
	Board = board_utils:create_board(10),
	say("0: ~n~p~n",[Board]),
	{ok, #state{board = Board, iteration = 0}}.

begin_work(Slaves) ->
	lists:foreach(fun(X) -> slave:begin_work(X) end, Slaves).

iterate(Iterations, State = #state{board = Board, iteration = Iteration}) ->
	%Active_nodes = discover_nodes(),
	Active_nodes = 3,
	Slaves = slave:start_slaves(Active_nodes),
	Slave_boards = board_utils:divide(Board, Active_nodes),
	Slaves_with_boards = lists:zip(Slaves, Slave_boards),
	%gen_server:cast(lists:nth(1,Slaves), {iterate, 1, Board, none, none, self()}),
	order_slaves(Iterations, Slaves_with_boards),
	begin_work(Slaves),
	{ok, State#state{iteration = Iteration + Iterations, slaves = Slaves, slaves_with_boards = Slaves_with_boards, response_count = Active_nodes}}.

order_slaves(Iterations, [H | T]) ->
	order_slaves(Iterations, none, H, T).

order_slaves(Iterations, none, {C_pid, C_board}, [{N_pid, N_board}|T]) ->
	slave:iterate(C_pid, Iterations, C_board, none, N_pid, self()),
	order_slaves(Iterations, {C_pid, C_board}, {N_pid, N_board}, T);

order_slaves(Iterations, {P_pid, _}, {C_pid, C_board}, []) ->
	slave:iterate(C_pid, Iterations, C_board, P_pid, none, self());

order_slaves(Iterations, {P_pid, _}, {C_pid, C_board}, [{N_pid, N_board}|T]) ->
	slave:iterate(C_pid, Iterations, C_board, P_pid, N_pid, self()),
	order_slaves(Iterations, {C_pid, C_board}, {N_pid, N_board}, T).


discover_nodes() ->
	lists:foldl(fun count_pongs/2, 0, lists:map(fun net_adm:ping/1,[l@le1, l@le2, l@le3, l@le4, l@le5, l@le6, l@le7, l@le8, l@le9, l@le10])).

count_pongs(pong, Sum) -> Sum + 1;
count_pongs(_, Sum) -> Sum.

%% Client API

next() -> Calc_Board = gen_server:call(?MODULE, {next, 1}).
next(N) ->
	Calc_Board = gen_server:call(?MODULE, {next, N}).

%% Public API

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop({local, ?MODULE}).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks

handle_call({next, Iterations}, From, State) ->
	{ok, New_state} = iterate(Iterations, State),
	{noreply, New_state#state{caller_pid = From}};

handle_call(stop, _From, State) ->
  %say("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast({result, Slave_pid, Board_frag}, State = #state{
	slaves_with_boards = Slaves_with_boards, response_count = Count} ) when Count > 1 ->
	Index = board_utils:find_slice_index(Slaves_with_boards, Slave_pid),
	New_slaves_with_boards = board_utils:replace_in_list(Index, Slaves_with_boards, {Slave_pid, Board_frag}),
	{noreply, State#state{slaves_with_boards = New_slaves_with_boards, response_count = Count - 1}};

handle_cast({result, Slave_pid, Board_frag}, State = #state{slaves = Slaves, slaves_with_boards = Slaves_with_boards, caller_pid = Caller} ) ->
	Index = board_utils:find_slice_index(Slaves_with_boards, Slave_pid),
	New_slaves_with_boards = board_utils:replace_in_list(Index, Slaves_with_boards, {Slave_pid, Board_frag}),
	%NewBoard = board_utils:merge(Slaves_with_boards),
	NewBoard = board_utils:merge(lists:map(fun({_, BoardFrag}) -> BoardFrag end, Slaves_with_boards)),
	slave:kill_slaves(Slaves),
	%say("NewBoard:~n~p~ndupa~n",[NewBoard]),
	gen_server:reply(Caller, NewBoard),
	{noreply, State#state{slaves_with_boards = New_slaves_with_boards, board = NewBoard}};

handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.




handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  %say("terminate ~p, ~p", [_Reason, _State]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).