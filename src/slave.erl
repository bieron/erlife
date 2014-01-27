-module(slave).
-compile(export_all).

-behaviour(gen_server).
-record(state, {board, new_board, operations_count = 0, iterations = 0, p_pid = none, n_pid = none, master_pid = none}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_slaves/1, kill_slaves/1]).

%% Client API
start_slaves(N) ->
  [Pid || {ok, Pid} <- [slave:start() || _ <- lists:seq(1,N)]].

kill_slaves(Pids) ->
  lists:foreach(fun stop/1, Pids).

setup(C_pid, Iterations, C_board, P_pid, N_pid, Master_pid) when Iterations =:= 0 ->
  {za_malo_iteracji};
setup(C_pid, Iterations, C_board, P_pid, N_pid, Master_pid) ->
  gen_server:cast(C_pid, {setup, Iterations, C_board, P_pid, N_pid, Master_pid}).

begin_work(Pid) ->
  gen_server:cast(Pid, begin_work).

%% Default API
start() ->
  gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

stop() ->
  stop(?MODULE).

state(Pid) ->
  gen_server:call(Pid, state).

%% Server implementation, a.k.a.: callbacks

init([]) ->
  {ok, #state{}}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call(state, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


% casts from master
handle_cast(begin_work, State = #state{new_board = NewBoard, iterations = Iterations, master_pid = Master_pid}) when Iterations =:= 0 -> 
  gen_server:cast(Master_pid, {result, self(), NewBoard}),
  {noreply, State};
handle_cast(begin_work, State = #state{board = Board}) -> 
  send_border_rows(State),
  NewBoard = calculate_middle(Board),
  NewState = State#state{new_board = NewBoard},
  NewState2 = check_if_ready(NewState),
  {noreply, NewState2};
handle_cast({setup, Iterations, Board, P_pid, N_pid, Master_pid}, State) ->
   {noreply, State#state{board = Board, new_board = Board, operations_count = determine_opc(N_pid,P_pid), iterations = Iterations, n_pid = N_pid, p_pid = P_pid, master_pid = Master_pid}};
% casts from other slaves
handle_cast({previous_row, Row}, State = #state{operations_count = Opc, board = Board, new_board = NewBoard}) ->
  NewState2 = calculate_border_row(Opc, Row, Board, NewBoard, State, fun calculate_first_row/3),
  {noreply, NewState2};
handle_cast({next_row, Row}, State = #state{operations_count = Opc, board = Board, new_board = NewBoard}) ->
  NewState2 = calculate_border_row(Opc, Row, Board, NewBoard, State,  fun calculate_last_row/3),
  {noreply, NewState2};
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

%% private API
send_border_rows(State = #state{p_pid = none, n_pid = N_pid, board = Board}) ->
  send_last_row(N_pid, Board);
send_border_rows(State = #state{n_pid = none, p_pid = P_pid, board = Board}) ->
  send_first_row(P_pid, Board);
send_border_rows(State = #state{n_pid = N_pid, p_pid = P_pid, board = Board}) ->
  send_first_row(P_pid, Board),
  send_last_row(N_pid, Board). 

send_last_row(Pid, Board) ->
  Last_row = board_utils:last_row(Board),
  gen_server:cast(Pid, {previous_row, Last_row}).

send_first_row(Pid, Board) ->
  First_row = board_utils:first_row(Board),
  gen_server:cast(Pid, {next_row, First_row}).

check_if_ready(State = #state{operations_count = Opc, iterations = I}) when Opc > 0 ->
  State#state{operations_count = Opc-1};

check_if_ready(State = #state{new_board = NewBoard, iterations = Iterations, p_pid = P_pid, n_pid = N_pid, operations_count = Opc}) ->
  begin_work(self()),
  State#state{board = NewBoard, iterations = Iterations-1, operations_count = determine_opc(P_pid,N_pid)}.

determine_opc(Pid1, Pid2) when Pid1 =:= none orelse Pid2 =:= none -> 1;
determine_opc(_,_) -> 2.

% Calculate boards functions
calculate_middle(Board) ->
  board_utils:iterate_2d_tuple_midarea(Board, fun board_utils:determine_cell_value/4).

calculate_border_row(Opc, Row, Board, NewBoard, State, Fun) ->
  NewBoard2 = Fun(Row, Board, NewBoard),
  NewState = State#state{board = Board, new_board = NewBoard2},
  check_if_ready(NewState).

calculate_last_row(Next_row, Board, NewBoard) ->
  Len = tuple_size(Board),
  BeforeLastRow = element(Len-1, Board),
  LastRow = element(Len, Board), 
  NewLastRow = board_utils:iterate_row(BeforeLastRow, LastRow, Next_row),
  setelement(Len, NewBoard, NewLastRow).

calculate_first_row(Previous_row, Board, NewBoard) ->
  AfterFirstRow = element(2, Board),
  FirstRow = element(1, Board),
  NewFirstRow = board_utils:iterate_row(AfterFirstRow, FirstRow, Previous_row),
  setelement(1, NewBoard, NewFirstRow).

%% helper functions

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).