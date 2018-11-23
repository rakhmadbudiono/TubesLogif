setEnemy(0) :- !.
setEnemy(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 4, E), mappingEnemy(E, X, Y), location(U, X, Y),
  random(1, 4, W), mappingWeapon(U, W), M is N-1, setEnemy(M), !.

mappingEnemy(1, X, Y) :- asserta(location(shinobiSuna, X, Y)), !.
mappingEnemy(2, X, Y) :- asserta(location(shinobiKiri, X, Y)), !.
mappingEnemy(3, X, Y) :- asserta(location(shinobiIwa, X, Y)), !.
mappingEnemy(4, X, Y) :- asserta(location(shinobiKumo, X, Y)), !.

mappingWeapon(X, 1) :- asserta(weapon(X, kunaiThrower)), !.
mappingWeapon(X, 2) :- asserta(weapon(X, shurikenThrower)), !.
mappingWeapon(X, 3) :- asserta(weapon(X, rasengan)), !.
mappingWeapon(X, 4) :- asserta(weapon(X, sexyNoJutsu)), !.
