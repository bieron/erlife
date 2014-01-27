-module(rw).
-compile(export_all).


make_fname() ->
   "data/" ++ lists:concat( tuple_to_list( time() ) ) ++ ".yolo".


tuple_to_string(Tuple) -> 
   lists:concat([element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))]).
tuple2d_to_string(Tuple) -> 
   [tuple_to_string(I) || I <- [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))]].
   

write(A) ->
   case file:open( make_fname(), [write,compressed] ) of
      {error, Reason} -> error(Reason);
      {ok, Fd} -> 
%         io:write(Fd,tuple_size(A)),  %tu niby tez moze byc blad - zapisu
         io:write(Fd,tuple_size(A))  %tu niby tez moze byc blad - zapisu
         
%            Strings = tuple2d_to_string(A),
%            lists:foreach(fun(S) -> master:say("~p", [S]) end, Strings), %i tu tez moze byc
 %           lists:foreach(fun(S) -> io:fwrite(Fd, "~p~n", [S]) end, Strings) %i tu tez moze byc
%         case io:write(Fd,tuple_size(A)) of %tu niby tez moze byc blad - zapisu
%         {error, Reason} -> error(Reason); %master:say("error: ~p~n", [Reason]) end;
%         {ok, Fd} ->
%            Strings = tuple2d_to_string(A),
%            lists:foreach(fun(S) -> master:say("~p", [S]) end, Strings), %i tu tez moze byc
%            lists:foreach(fun(S) -> io:fwrite(Fd, "~p~n", [S]) end, Strings) %i tu tez moze byc
%         end
   end,
      master:say("~p~n",[Fd]).
%   {ok,Fd} = file:open( make_fname(), [write,compressed] ),
%   io:write(Fd,A), 
%   file:write_file( make_fname(), A).
