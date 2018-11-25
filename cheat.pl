/* CHEAT */

/* HP = 9999 */
baguvix :-  retract(health(self,_)),
			asserta(health(self,9999)),
			write("Cheat aktif!"), nl.
			
/* HP = 0 */
goodbyecruelworld :- 	retract(health(self,_)),
						asserta(health(self,0)),
						write("Cheat aktif!"),nl,
						write("Kenapa lu cheat ini wkwk!!!"), nl.

/* Reset Deadzone */
delDeadzone :- retract(location(deadzone,X,Y))).
delDeadzone :- location(_,_,_).
setMapCheat(_, X, Y) :- setPixel(X, Y, '-').
setMapCheat(_, _, _) :- location(_,_,_).

aezakmi :- forall(between(2,9,Y),
			   forall(between(2,9,X), (
				  location(Q,X,Y), setMapCheat(Q,X,Y)
			   )
			)), location(self, X, Y), setPixel(X,Y,'P'),delDeadzone, !.
			write("Cheat Aktif!"), nl.

/* Super Punch */
stinglikeabee :- 	retract(damage(none,5)),
					asserta(damage(none,100)),
					write("Cheat Aktif!"), nl.
/* Sennin Mode = Powerful Rasengan */
worshipme :-	retract(weapon(self,_)),
				retract(damage(rasengan,20)),
				asserta(weapon(self,rasengan)),
				asserta(damage(rasengan,9999)),
				write("Cheat Aktif!"), nl.

/* Senjata Terkuat */
uzumymw :-	retract(weapon(self,_)),
			asserta(weapon(self,ultimateJutsu)),
			write("Cheat Aktif!"), nl.

/* Armor Terkuat */
hesoyam :-	retract(armor(self,_,_)),
			asserta(armor(self,nonNewtonianFluidArmor,9999)),
			write("Cheat Aktif!"), nl.
			
/* Semua Attack kunai dan shuriken jadi 9999 */
professionalkiller :-	retract(damage(kunaiThrower, 10)),
						retract(damage(shurikenThrower, 10)),
						asserta(damage(kunaiThrower, 9999)),
						asserta(damage(shurikenThrower, 9999)),
						write("Cheat Aktif!"), nl.
						
/* Sexy Jutsu jadi ampuh */
helloladies :-	retract(damage(sexyNoJutsu, 25)),
				asserta(damage(sexyNoJutsu, 9999)),
				write("Cheat Aktif!"), nl.

/* Musuh jadi kuat */
bringiton :-	retract(health(shinobiSuna,_)),
				retract(health(shinobiKiri,_)),
				retract(health(shinobiIwa,_)),
				retract(health(shinobiKumo,_)),
				retract(damage(shinobiSuna,5)),
				retract(damage(shinobiKiri,5)),
				retract(damage(shinobiIwa,5)),
				retract(damage(shinobiKumo,5)),
				asserta(health(shinobiSuna,100)),
				asserta(health(shinobiKiri,100)),
				asserta(health(shinobiIwa,100)),
				asserta(health(shinobiKumo,100)),
				asserta(damage(shinobiSuna,100)),
				asserta(damage(shinobiKiri,100)),
				asserta(damage(shinobiIwa,100)),
				asserta(damage(shinobiKumo,100)),
				write("Cheat Aktif!"), nl.
				write("Greget juga lu wkwk.")

/* Teleport */
behindyou(newX,newY) :-	retract(location(self,_,_)),
			asserta(location(self,newX,newY)),
			write("Hiraishin no Jutsu!"), nl,
			write("Cheat Aktif!"), nl.
