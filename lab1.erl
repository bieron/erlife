-module(lab1).
-export([area/1]).
-export([vol/1]).
-export([len/1]).
-export([len/2]).
-export([mx/1]).
-export([mx/2]).
-export([mn/1]).
-export([mn/2]).
-export([min_max/1]).
-export([max_min/1]).
-export([areaList/1]).
-export([dec/1]).
-export([dec/2]).
-export([tmc/2]).
     
area({rect,X,Y}) ->
	X*Y;
area({cir,X}) -> 3.14*X*X;
area({tri,X,Y}) -> X*Y/2;
area({cub,X}) -> 6*X;
area({con,H,R}) -> (R*R+H)*3.14;
area({sph,R}) -> 4*R*R*3.14;
area({del,X,Y}) -> (X+Y)/2.
vol({sph,R}) -> 4.19*R*R*R;
vol({cub,R}) -> R*R*R;
vol({con,H,R}) -> R*R*3.14*H/3.

len([_|T]) -> len(T,1).
len([_|T], X) -> len(T,X+1);
len([],X) -> X.

mx([H|T]) -> mx(H,T).
mx(M,[]) -> M;
mx(M,[H|L]) when M > H -> mx(M,L);
mx(_M,[H|L]) -> mx(H,L).

mn([H|T]) -> mn(H,T).
mn(M,[]) -> M;
mn(M,[H|L]) when M < H -> mn(M,L);
mn(_M,[H|L]) -> mn(H,L).

min_max(L) -> {mx(L), mn(L)}.
max_min(L) -> [mx(L), mn(L)].

areaList(L) -> [area(X) || X <- L].

dec(X) when X =:= 1 -> [1];
dec(X) -> dec(X, [1]).
dec(X,[H|L]) when H =:=X -> [H|L];
dec(X,[H|L]) -> dec(X,[H+1] ++ [H|L]).

%dec(X) -> dec([X],X-1).
%dec([X],Y) when Y =:= 1 -> [X] ++ [Y];
%dec([X],Y) -> dec([X],Y-1) ++ [Y].

tmc({c,X},k) -> {k, X-273};
tmc({k,X},c) -> {c, X+273};
tmc({c,X},f) -> {f, X*9/5+32};
tmc({f,X},c) -> {c, (X-32)*5/9};
tmc({k,X},f) -> tmc(tmc({k,X},c),f);
tmc({f,X},k) -> tmc(tmc({f,X},c),k).


%powie mi ze X jest unbound
%Tab = [{c,k, X-273},{k,c,X+273},{c,f,X*9/5+32},{f,c,(X-32)*5/9},{k,f,(X-273)*9/5+23},{f,k,(X-32)*5/9+273}].
%temc({S,X},D) ->  

%temc({c,X},k) -> 
