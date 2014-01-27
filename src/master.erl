-module(master).
-compile(export_all).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {board, iteration, slaves, slaves_with_boards, response_count, callerpid}).
-define(TIMEOUT, 20000).

%% Public API

start() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop({global, ?MODULE}).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state({global, ?MODULE}).

next() -> 
	gen_server:call({global, ?MODULE}, {next, 1}, ?TIMEOUT).
next(N) ->
	gen_server:call({global, ?MODULE}, {next, N}, ?TIMEOUT).

%% private implementation
init([]) ->
	discover_nodes(),
	c:nl(master),
	c:nl(slave),
	c:nl(board_utils),
	c:nl(benchmark),
	Board = board_utils:create_board(15),
	{ok, #state{board = Board, iteration = 0}}.

setup(Iterations, State = #state{board = Board, iteration = Iteration}) ->
	{NodesCount, Nodes} = discover_nodes(),
	Slaves = slave:start_slaves(Nodes),
	Slave_boards = board_utils:divide(Board, NodesCount),
	Slaves_with_boards = lists:zip(Slaves, Slave_boards),
	say("slaves with boards ~n~p~n", [Slaves_with_boards]),
	order_slaves(Iterations, Slaves_with_boards),
	begin_work(Slaves),
	{ok, State#state{iteration = Iteration + Iterations, slaves = Slaves, slaves_with_boards = Slaves_with_boards, response_count = NodesCount}}.

begin_work(Slaves) ->
	lists:foreach(fun(X) -> slave:begin_work(X) end, Slaves).

order_slaves(Iterations, [H | T]) ->
	order_slaves(Iterations, {none, none}, H, T).

order_slaves(Iterations, {none, none}, {{Cnode, Cpid}, Cboard}, [{{Nnode, Npid}, Nboard}|T]) ->
	rpc:cast(Cnode, slave, setup, [Cpid, Iterations, Cboard, {none, none}, {Nnode, Npid}, {node(), self()}]),
	%slave:setup(Cpid, Iterations, Cboard, none, Npid, self()),
	order_slaves(Iterations, {{Cnode, Cpid}, Cboard}, {{Nnode, Npid}, Nboard}, T);
order_slaves(Iterations, {{Pnode, Ppid}, _}, {{Cnode, Cpid}, Cboard}, []) ->
	rpc:cast(Cnode, slave, setup, [Cpid, Iterations, Cboard, {Pnode, Ppid}, {none, none}, {node(), self()}]);
	%slave:setup(Cpid, Iterations, Cboard, Ppid, none, self());
order_slaves(Iterations, {{Pnode, Ppid}, _}, {{Cnode, Cpid}, Cboard}, [{{Nnode, Npid}, Nboard}|T]) ->
	rpc:cast(Cnode, slave, setup, [Cpid, Iterations, Cboard, {Pnode, Ppid}, {Nnode, Npid}, {node(), self()}]),
	%slave:setup(Cpid, Iterations, Cboard, Ppid, Npid, self()),
	order_slaves(Iterations, {{Cnode, Cpid}, Cboard}, {{Nnode, Npid}, Nboard}, T).

discover_nodes() ->
	Names = [l@le1, l@le2, l@le3, l@le4, l@le5, l@le6, l@le7, l@le8, l@le9, l@le10],
	%Nodes = lists:map(fun net_adm:ping/1,[l@le1, l@le2, l@le3, l@le4, l@le5, l@le6, l@le7, l@le8, l@le9, l@le10]),
	Nodes = lists:foldl(fun(N, Nds) -> filter_nodes(N, Nds, net_adm:ping(N)) end, [], Names),
	Count = length(Nodes),
	{Count, Nodes}.

filter_nodes(N, Nodes, pong) -> [N | Nodes];
filter_nodes(_N, Nodes, _) -> Nodes.


count_pongs(pong, Sum) -> Sum + 1;
count_pongs(_, Sum) -> Sum.

%% Server implementation, a.k.a.: callbacks
% synchronous callbacks
handle_call({next, Iterations}, From, State) ->
	{ok, New_state} = setup(Iterations, State),
	{noreply, New_state#state{callerpid = From}};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};
handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.

% asynchronous callbacks
handle_cast({result, Slave, Board_frag}, State = #state{
	slaves_with_boards = Slaves_with_boards, response_count = Count} ) when Count > 1 ->
	Index = board_utils:find_slice_index(Slaves_with_boards, Slave),
	New_slaves_with_boards = board_utils:replace_in_list(Index, Slaves_with_boards, {Slave, Board_frag}),
	say("got result ~n~p~n", [Board_frag]),
	{noreply, State#state{slaves_with_boards = New_slaves_with_boards, response_count = Count - 1}};
handle_cast({result, Slave, Board_frag}, State = #state{slaves = Slaves, slaves_with_boards = Slaves_with_boards, callerpid = Caller} ) ->
	Index = board_utils:find_slice_index(Slaves_with_boards, Slave),
	New_slaves_with_boards = board_utils:replace_in_list(Index, Slaves_with_boards, {Slave, Board_frag}),
	NewBoard = board_utils:merge(lists:map(fun({_, BoardFrag}) -> BoardFrag end, New_slaves_with_boards)),
	%slave:kill_slaves(Slaves),
	gen_server:reply(Caller, NewBoard),
	say("got result ~n~p~n", [Board_frag]),
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