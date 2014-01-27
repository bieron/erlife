erlife - rozproszone erlangowe Game of Life
======
Na branchu master nasz Erlang Game of Life jest rozproszony na wątkach na lokalnym hoscie. 

Kompilujemy poleceniem `erl -make` wywołanym z katalogu `erlife/`.
Kompilowane są źródła z katalogu `erlife/src`, binarki znajdują się w `erlife/ebin`.

Po skompilowaniu, odpalamy konsolę erlanga poleceniem `erl` i ładujemy moduł `master`, odpalamy funkcję `start()`, a potem możemy już wywoływać `next() `lub `next(N)` z załadowanego modułu, które zwrócą nam stan tablicy po kolejno 1 lub N iteracjach.

Przykładowe wywołanie:
`l(master).
master:start().
master:next().`
