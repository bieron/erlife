Rozproszone Erlang Game of Life
======
Wersja przygotowana do działania na udostępnionych Node'ach z serwera `borg.kis.agh.edu.pl`.

Kompilujemy poleceniem `erl -make` wywołanym z katalogu `erlife/`.
Kompilowane są źródła z katalogu `erlife/src`, binarki znajdują się w `erlife/ebin`.

Po skompilowaniu, odpalamy konsolę erlanga poleceniem `erl -sname login -setcookie pwir` z katalogu `erlife/ebin`.

Użycie:

`l(starter).`

`starter:start(N). %gdzie dim(plansza) =:= 2**N , tworzy losowo plansze`

`starter:read_and_start(Filename). %zamiast powyższego!`

`starter:next(). %zwroc kolejna iteracje planszy`

`starter:next(L). %zwroc L-tą iterację planszy`

`benchmark:test_time([X],Y). % gdzie X: ilosc iteracji do obliczenia przez slave'y, Y: ilosc liczonych prob przez benchmark`

`benchmark:test_time([X]) → test_time([X],1).`

`benchmark:test_time() → test_time([10],10).`

`master_bula:say("~p~n", ["to jest życie!"]).`

`master_bula:save().%zapisuje bieżącą, wyliczoną tablicę do fff.gz w working-dir`

Więcej informacji w pliku `erlang_readme.pdf`
