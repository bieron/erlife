-module(lifeio).
-export([lifeRead/1,readData/2,lifeWrite/2,writeData/2,testWrite/1,testRead/1]).

%% otwarcie pliku do wczytywania
%% zwracany jest deskryptor pliku i rozmiar danych/planszy
lifeRead(FileName) ->
		{ok,FD} = file:open(FileName,[read,compressed]),
		case file:read(FD,1) of 
				{ok,[Data]} -> {FD,Data};
				eof -> io:format("~nKoniec~n",[]);
				{error,Reason} -> io:format("~s~n",[Reason])
		end.

%% odczytanie kolejnej porcji danych o żądanym rozmiarze
readData(FD,Length) -> 
		case file:read(FD,Length) of 
				{ok,Data} -> {Data};
				eof -> io:format("~nKoniec~n",[]);
				{error,Reason} -> io:format("~s~n",[Reason])
		end.

%% otwarcie pliku do zapisu planszy o wskazanym rozmiarze
lifeWrite(FileName,Size)->
		{ok,FD} = file:open(FileName,[write,compressed]),
		file:write(FD,Size),
		{ok,FD}.

%% zapisanie kolejnej porcji danych
writeData(FD,Data) ->
		file:write(FD,Data).

%% procedura testowa zapisująca losową plansze
%% o wskazanym rozmiarze
testWrite(Size) ->
		Len = trunc(math:pow(2,Size)),
		{ok,FD} = lifeWrite('fff.gz',8),
		file:write(FD,[Size]),
		feedData(FD,Len,Len),
		file:close(FD).

feedData(_FD,0,_Len)-> ok;
feedData(FD,Count,Len) ->
		Data = [random:uniform(2)+48 || _ <- lists:seq(1, Len)],
		writeData(FD,Data),
		feedData(FD,Count-1,Len).


%% procedura testowa odczytująca planszę z pliku
testRead(FileName) ->
		{FD,Size} = lifeRead(FileName),
		Len = trunc(math:pow(2,Size)),
		io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
		getData(FD,Len,Len),
		file:close(FD).


getData(_FD,_Len,0) -> ok;
getData(FD,Len,Count) ->
		readData(FD,Len),
		getData(FD,Len,Count-1).