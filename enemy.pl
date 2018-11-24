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

enemyMovement(X, K, L) :- mappingEnemyMovement(X, 2, K, L).

mappingEnemyMovement(_, 1, _, _) :- !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A > C, B > D,
  Cnew is C+1, Dnew is D+1, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A > C, B == D,
  Cnew is C+1, Dnew is D, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A > C, B < D,
  Cnew is C+1, Dnew is D-1, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A == C, B > D,
  Cnew is C, Dnew is D+1, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A == C, B == D,
  Cnew is C, Dnew is D, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A == C, B < D,
  Cnew is C, Dnew is D-1, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A < C, B > D,
  Cnew is C+1, Dnew is D+1, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A < C, B == D,
  Cnew is C+1, Dnew is D, asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, 2, C, D) :-  location(self, A, B), retract(location(X, C, D)), A < C, B < D,
  Cnew is C+1, Dnew is D-1, asserta(location(X, Cnew, Dnew)), !.

attackPlayer(X) :- location(X, A, B), location(self, A, B), retract(health(self, H)), weapon(X, W),
  weaponList(W, D), HP is H-D, asserta(health(self, HP)), write(X), write(" baru saja menyerangmu dengan "),
  write(W), write(". Darahmu berkurang sebanyak "), write(D).

attackPlayer(_).

enemiesTurn :- forall(enemy(X), forall(location(X, A, B), (enemyMovement(X, A, B), attackPlayer(X)))), !.
