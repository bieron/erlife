-module(slave).
-compile(export_all).

-behaviour(gen_server).
-record(state, {board, neighbors_count}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_slaves/1, kill_slaves/1]).

%% Client API
start_slaves(N) ->
  list_to_tuple([{Pid} || {ok, Pid} <- [slave:start() || _ <- lists:seq(1,N)]]).

kill_slaves(Pids) ->
  lists:map(fun stop/1, Pids).

iterate(C_pid, Iterations, C_board, P_pid, N_pid) ->
  gen_server:cast(C_pid, {iterate, Iterations, C_board, P_pid, N_pid}).


%% Public API

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
  say("init", []),
  {ok, []}.


handle_call(stop, _From, State) ->
  say("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.

% casts from master
handle_cast({iterate, Iterations, Board, none, N_pid}, State = #state{}) ->
  send_last_row(N_pid, Board),
  calculate_middle(Board),
  {noreply, State#state{board = Board, neighbors_count = 1}};
handle_cast({iterate, Iterations, Board, P_pid, none}, State = #state{}) ->
  send_first_row(P_pid, Board),
  calculate_middle(Board),
  {noreply, State#state{board = Board, neighbors_count = 1}};
handle_cast({iterate, Iterations, Board, P_pid, N_pid}, State = #state{}) ->
  send_last_row(N_pid, Board),
  send_first_row(P_pid, Board),
  calculate_middle(Board),
  {noreply, State#state{board = Board, neighbors_count = 2}};

% casts from other slaves
handle_cast({previous_row, Row}, State = #state{neighbors_count = Nbc, board = Board}) ->
  calculate_first_row(Row, Board),
  {noreply, State#state{neighbors_count = Nbc - 1}};
handle_cast({next_row, Row}, State = #state{neighbors_count = Nbc, board = Board}) ->
  calculate_last_row(Row, Board),
  {noreply, State#state{neighbors_count = Nbc - 1}};

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
send_last_row(Pid, Board) ->
  Last_row = board_utils:last_row(Board),
  gen_server:cast(Pid, {previous_row, Last_row}).

send_first_row(Pid, Board) ->
  First_row = board_utils:first_row(Board),
  gen_server:cast(Pid, {next_row, First_row}).

calculate_middle(Board) ->
  board_utils:iterate_2d_tuple(Board, fun determine_cell_value/4).

calculate_last_row(Next_row, Board) ->
  true.

calculate_first_row(Previous_row, Board) ->
  true.

%% helper functions

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).