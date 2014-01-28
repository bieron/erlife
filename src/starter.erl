-module(starter).
-export([start/1, next/1]).

start(BoardSize) ->
	c:l(master_bula),
	c:l(slave_bula),
	c:nl(lifeio),
	lifeio:testWrite(BoardSize),
	master_bula:start(),
	c:nl(slave_bula).

next(N) ->
	master_bula:next(N).