Rozproszone Erlang Game of Life
======
Wersja przygotowana do działania na udostępnionych Node'ach z serwera borg.kis.agh.edu.pl.

Kompilujemy poleceniem `erl -make` wywołanym z katalogu `erlife/`.
Kompilowane są źródła z katalogu `erlife/src`, binarki znajdują się w `erlife/ebin`.

Po skompilowaniu, odpalamy konsolę erlanga poleceniem `erl -sname login -setcookie pwir` z katalogu `erlife/ebin`.

Przykładowe wywołanie:
`l(master).
master:start().
master:next().`

'l(starter).
> starter:start(N).
%gdzie dim(plansza) =:= 2**N , tworzy losowo plansze
> starter:read_and_start(Filename). %zamiast powyższego!
> starter:next().
%zwroc kolejna iteracje planszy
> starter:next(L).
%zwroc L-tą iterację planszy
> benchmark:test_time([X],Y). % gdzie
X: ilosc iteracji do obliczenia przez slave'y
Y: ilosc liczonych prob przez benchmark
> benchmark:test_time([X]) → test_time([X],1).
> benchmark:test_time() → test_time([10],10).
> master_bula:say("~p~n", ["to jest życie!"]).
> master_bula:save().
%zapisuje bieżącą, wyliczoną tablicę do fff.gz w working-dir
'

Istnieje opcja sprawdzania czasu wykonania się jednej iteracji poprzez funkcję `benchmark:test_time([N],M).`, gdzie N to ilość wywołania jednej iteracji, a M to ilość iteracji. `test_time/2` zwróci w pierwszym przypadku minimalny, maksymalny i średni czas liczenia jednej itereacji, a w drugim przypadku minimalny, maksymalny i średni czas liczenia M iteracji
