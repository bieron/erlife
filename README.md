Rozproszone Erlang Game of Life
======
Na branchu master nasz Erlang Game of Life jest rozproszony na wątkach na lokalnym hoscie. 

Kompilujemy poleceniem `erl -make` wywołanym z katalogu `erlife/`.
Kompilowane są źródła z katalogu `erlife/src`, binarki znajdują się w `erlife/ebin`.

Po skompilowaniu, odpalamy konsolę erlanga poleceniem `erl` z katalogu `erlife/ebin`  i już z poziomu erlangowej konsoli ładujemy moduł `master`, odpalamy funkcję `start()`, a potem możemy już wywoływać `next() `lub `next(N)` z załadowanego modułu, które zwrócą nam stan tablicy po kolejno 1 lub N iteracjach.

Przykładowe wywołanie:
`l(master).
master:start().
master:next().`

Istnieje opcja sprawdzania czasu wykonania się jednej iteracji poprzez funkcję `benchmark:test_time([N],M).`, gdzie N to ilość wywołania jednej iteracji, a M to ilość iteracji. `test_time/2` zwróci w pierwszym przypadku minimalny, maksymalny i średni czas liczenia jednej itereacji, a w drugim przypadku minimalny, maksymalny i średni czas liczenia M iteracji
