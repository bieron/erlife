-module(lifeio).
-export([lifeRead/1,readData/2,lifeWrite/2,writeData/2,testWrite/1,testRead/1,readOutputToTuple/1]).

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
				{ok,Data} -> Data;
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
		{ok,FD} = lifeWrite('fff.gz',Size),
		file:write(FD,[Size]),
		feedData(FD,Len,Len),
		file:close(FD).

feedData(_FD,0,_Len)-> ok;
feedData(FD,Count,Len) ->
		Data = [random:uniform(2)-1 + 48 || _ <- lists:seq(1, Len)],
		writeData(FD,Data),
		feedData(FD,Count-1,Len).


%% procedura testowa odczytująca planszę z pliku
testRead(FileName) ->
		{FD,Size} = lifeRead(FileName),
		Len = trunc(math:pow(2,Size)),
		io:fwrite("Rozmiar ~B, Plansza ~Bx~B~n",[Size,Len,Len]),
		Data = getData(FD,Len,Len),
		file:close(FD),
		{Data, Size}.


getData(FD,Len,Count) -> getData(FD,Len,Count,[]).

getData(_FD,_Len,0,Data) -> 
		list_to_tuple(lists:reverse(Data));
getData(FD,Len,Count,Data) ->
		Data2 = readOutputToTuple(readData(FD,Len)),
		Data3 = [Data2|Data],
		getData(FD,Len,Count-1,Data3).



% stringToIntegerTuple(String,Count) -> stringToIntegerTuple(String,Count,[]).

% stringToIntegerTuple(_String,0,[Result]) -> 
% 		list_to_tuple(Result);
% stringToIntegerTuple(String,Count,[Result]) -> 
% 		{Result2,_} = string:to_integer(string:left(String,1)),
% 		Result3 = Result ++ [Result2],
% 		stringToIntegerTuple(String,Count-1,Result3).

readOutputToTuple(Data) ->
	readOutputToTuple(Data,[],length(Data)).

readOutputToTuple(Data, Acc, 0) -> list_to_tuple(Acc);
readOutputToTuple(Data, Acc, Len) ->
Data1 = string:right(Data, Len-1),
{Data2, Rest2} = string:to_integer(string:left(Data,1)),
	readOutputToTuple(Data1,lists:append(Acc,[Data2]), Len-1).