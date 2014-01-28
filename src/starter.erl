-module(starter).
-export([start/1, next/1, next/0, stop/0]).

start(BoardSize) ->
	c:l(master_bula),
	c:l(slave_bula),
	c:nl(lifeio),
	lifeio:testWrite(BoardSize),
	master_bula:start(),
	c:nl(slave_bula).

stop() ->
	master_bula:stop().

next() ->
	next(1).

next(N) ->
	master_bula:next(N).