-module(starter).
-export([start/1, next/1, next/0, stop/0, read_and_start/1]).

start(BoardSize) ->
	init(),
	lifeio:testWrite(BoardSize),
	master_bula:start(),
	c:nl(slave_bula).

read_and_start(FileName) ->
	init(). % tu zmien . na ,
	%master_bula:start(FileName).

init() ->
	c:l(master_bula),
	c:l(slave_bula),
	c:nl(lifeio),
	c:nl(master_bula),
	c:nl(slave_bula),
	c:nl(board_utils_bula),
	c:nl(benchmark).

stop() ->
	master_bula:stop().

next() ->
	next(1).

next(N) ->
	master_bula:next(N).