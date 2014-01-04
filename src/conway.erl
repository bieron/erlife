-module(conway).
%-export([gof/3,timer/1,cell/6]).
-export([make_cell/2, calc_state/2,init_board/3,replace_nth/3]).
-export([tick/2]).


make_cell(State,Neighbors) -> {cell, 1,1, State, Neighbors}.

%calc_state({cell, X,Y,State,Neighbors}) ->
calc_state(State,Neighbors) ->
   if
        State==alive , Neighbors < 2 -> dead;
        State==alive , Neighbors < 4 -> alive;
        true -> dead
   end.

%init_board(W,H) -> create_board_acc(W,H,0,[]).
init_board(W,H,Alive) -> 
   B = create_board_acc(W,H,0),
   raise_dead(W, Alive, B).

%create_board_acc(W,H,I,B) when I==W*H -> B;
%create_board_acc(W,H,I,B) ->
%   [create_cell(I rem H, I div W) | create_board_acc(W,H,I+1)].

create_board_acc(W,H,I) when I==W*H -> [];
create_board_acc(W,H,I) ->
   [create_cell(I rem H, I div W) | create_board_acc(W,H,I+1)].

create_cell(X,Y) -> {cell, X,Y,dead,unset,0}.

%calc_state(B, 


raise_dead(_,[],B) -> B;
raise_dead(W,[{X,Y}|Alive],B) ->
%   lists:nth(Y*W+X,B),
%   raise_dead(W,Alive,B).
   raise_dead(W, Alive, replace_nth(Y*W+X,{cell,X,Y,alive,unset},B)).

replace_nth(_, _, []) -> [];
replace_nth(N, New, List) ->
   replace_nth(N, New, [], List).
replace_nth(0, New, Left, [_|Right]) ->
   Left ++ [New] ++ Right;
replace_nth(N, New, Left, [T|Right]) ->
   replace_nth(N-1, New, Left++[T], Right).


tick(B, Interval) ->
   timer:sleep(Interval).


%usage example
%init_board(5,5,[{1,1},{2,0},{4,1}]).
