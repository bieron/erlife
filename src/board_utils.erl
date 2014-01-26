-module(board_utils).
-compile([export_all]).

%divide(Board, N) ->
%	.

%merge(Boards) ->
%	.

last_row(Board) ->
	element(tuple_size(Board), Board).

first_row(Board) ->
	element(1,Board).

divide(Board, Workers) ->
	divide(Board, division_points(tuple_size(Board), Workers), []).

merge(Areas) ->
	merge(Areas, []).

create_board(Size) -> 
	list_to_tuple([list_to_tuple([random:uniform(2)-1 || _ <- lists:seq(1,Size)]) || _ <- lists:seq(1, Size)]).

iterate_2d_tuple_midarea(Board, Fun) ->
  Row_len = tuple_size(Board),
  Col_len = tuple_size(element(1,Board)),
  iterate_2d_tuple_midarea(Board, Board, 2, 2, Row_len, Col_len, Fun).

%% implementations

iterate_2d_tuple_midarea(_, New_board, Curr_row, Curr_col, Row_len, Col_len, _) when Curr_row =:= Row_len - 1 andalso Curr_col =:= Col_len ->
  New_board;
iterate_2d_tuple_midarea(Board, New_board, Curr_row, Curr_col, Row_len, Col_len, Fun) when Curr_col =:= Col_len ->
  iterate_2d_tuple_midarea(Board, New_board, Curr_row+1, 2 , Row_len, Col_len, Fun);

iterate_2d_tuple_midarea(Board, New_board, Curr_row, Curr_col, Row_len, Col_len, Fun) when Curr_col < Col_len ->
  New_board2 = Fun(Board, New_board, Curr_row, Curr_col),
  iterate_2d_tuple_midarea(Board, New_board2, Curr_row, Curr_col + 1, Row_len, Col_len, Fun).

determine_cell_value(Board, New_board, Cur_col, Cur_row) ->
  CurrValue = element(Cur_col, element(Cur_row, Board)),
  AliveNeighbors =  element(Cur_col-1, element(Cur_row+1, Board)) +
                    element(Cur_col-1, element(Cur_row, Board)) +
                    element(Cur_col-1, element(Cur_row-1, Board)) +

                    element(Cur_col, element(Cur_row+1, Board)) +
                    element(Cur_col, element(Cur_row-1, Board)) +

                    element(Cur_col+1, element(Cur_row+1, Board)) +
                    element(Cur_col+1, element(Cur_row, Board)) +
                    element(Cur_col+1, element(Cur_row-1, Board)),

  NewValue = if AliveNeighbors =:= 3 ->  1;
              AliveNeighbors =:= 2 andalso CurrValue =:= 1 -> 1;
              true -> 0
            end,                    
  setelement(Cur_row, New_board, setelement(Cur_col, element(Cur_row, New_board), NewValue)).

% divide

divide(_Board, [_], Acc) ->
	lists:reverse(Acc);
divide(Board, [First,Second|T], Acc) ->
	divide(Board, [Second|T], [subtuple(Board,First+1,Second)|Acc]).

subtuple(Tuple, First, Second) ->
	subtuple(Tuple, First, Second, []).
subtuple(Tuple, First, First, Acc) ->
	list_to_tuple([element(First, Tuple)|Acc]);
subtuple(Tuple, First, Second, Acc) when First < Second ->
	subtuple(Tuple, First, Second-1, [element(Second, Tuple)|Acc]).

division_points(Rows, Workers) when Workers > Rows ->
	too_many_workers;
division_points(Rows, Workers) ->
	Rows_per_worker = Rows div Workers,
	Remainder = Rows rem Workers,
	Basic_division = lists:duplicate(Workers, Rows_per_worker),
	Remainder_division = lists:duplicate(Remainder, 1) ++ lists:duplicate(Workers - Remainder, 0),
	Division = lists:zipwith(fun(X, Y) -> X+Y end, Basic_division, Remainder_division),
	cumsum([0|Division]).

cumsum(List) ->
	cumsum(List, [], 0).
cumsum([], Acc, _Acc2) ->
	lists:reverse(Acc);
cumsum([H|T], Acc, Acc2) ->
	cumsum(T, [H+Acc2|Acc], Acc2+H).

% merge

merge([], Board) ->
	list_to_tuple(lists:reverse(Board));
merge([H|T], Board) ->
	merge(T, jointolist(H, Board)).

jointolist(Tuple, List) ->
	jointolist(1, Tuple, List).
jointolist(I, Tuple, List) when I > tuple_size(Tuple) ->
	List;
jointolist(I, Tuple, List) ->
	jointolist(I+1, Tuple, [element(I, Tuple)|List]).


