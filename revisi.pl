/*tambahin ke start*/
	asserta(ammo(c,3)),
	asserta(ammo(k,3)),
	asserta(ammo(s,3)),
	asserta(ammo(p,3)),
	
/*tambahin ke use*/
use(X) :- ammoList(X), inventory(I), member(X,I), retract(inventory(I)), delete_one(X, I, Y), assertz(inventory(Y)), assocAmmo(X,P), retract(ammo(P,O)), Temp is O + 5, assertz(ammo(P,Temp)),!.
assocAmmo(chakra,c).
assocAmmo(kunai,k).
assocAmmo(shuriken,s).
assocAmmo(scroll,p).

/*attack*/

/*overwrite attack*/

attack :- \+ game(1), write('Kamu belum memulai permainan'), !.
attack :- enemy(E), location(self,X,Y), location(E,X,Y), weapon(self,none), write("Anda tidak punya senjata, jadi anda meninjunya!"), nl, attackR(E), !.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), W == 0, write("Ammomu kosong!"), nl, retract(weapon(self,Q)), asserta(weapon(self,none)), attackR(E), retract(weapon(self,none)), asserta(weapon(self,Q)),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), health(E, H), H == 0, retract(ammo(R,W)), W1 is W-1, assertz(ammo(R,W1)),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), attackR(E), retract(ammo(R,W)), W1 is W-1, assertz(ammo(R,W1)), !.
ammospec(kunaiThrower,k).
ammospec(shurikenThrower,s).
ammospec(rasengan,c).
ammospec(sexyNoJutsu,p).
ammospec(ultimateJutsu,c).


/*Overwrite status*/

status :- game(0), write('Kau belum memulai permainan.'), fail, !.
status :- game(1), health(self, HE), write('Darah : '), write(HE), nl,
	inventory(L), weapon(self,B), write('Senjata : '), write(B), nl, armor(self,W,H), write('Armor: '), write(W), nl, write('Proteksi: '), write(H), nl,
	write('Inventori : '), writestatinv(L), nl, write('Cakra: '), ammo(c,T1), write(T1), nl, write('Kunai: '), ammo(k,T2), write(T2), nl, write('Shuriken: '), ammo(s,T3), write(T3), nl,  write('Scroll: '), ammo(p,T4), write(T4),!.

writestatinv(L):- inventory(L), L == [], write('Anda tidak punya apa-apa'),!.
writestatinv(L):- write(L),!.