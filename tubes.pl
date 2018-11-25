/* Nama Kelompok : Prolog > Analog
Kelas : K-01
Nama Anggota :
  - Ricky Yuliawan 13517025
  - Rayza Mahendra Guntara Harsono 13517073
  - Muhammad Al Terra 13517145
  - Rakhmad Budiono 13517151 */

/* Fact */
:- dynamic(count/1).
:- dynamic(dead/1).
:- dynamic(location/3).
:- dynamic(nEnemy/1).
:- dynamic(game/1).
:- dynamic(health/2).
:- dynamic(inventory/1).
:- dynamic(isInventFull/1).
:- dynamic(weapon/2).
:- dynamic(armor/3).
:- dynamic(ammo/2).

dead(1).
count(0).
game(0).

enemy(shinobiSuna).
enemy(shinobiKiri).
enemy(shinobiIwa).
enemy(shinobiKumo).

weaponList(kunaiThrower).
weaponList(shurikenThrower).
weaponList(rasengan).
weaponList(sexyNoJutsu).
weaponList(ultimateJutsu).

armorList(ironArmor, 50).
armorList(woodArmor, 25).
armorList(nonNewtonianFluidArmor, 100).

medicineList(katsuyu).
medicineList(healingJutsu).

ammoList(kunai).
ammoList(scroll).
ammoList(shuriken).
ammoList(chakra).

health(shinobiSuna, 50).
health(shinobiKiri, 50).
health(shinobiIwa, 50).
health(shinobiKumo, 50).

allName(shinobiSuna, 'Shinobi Suna').
allName(shinobiKiri, 'Shinobi Kiri').
allName(shinobiIwa, 'Shinobi Iwa').
allName(shinobiKumo, 'Shinobi Kumo').
allName(katsuyu, 'Katsuyu').
allName(healingJutsu, 'Healing Jutsu').
allName(kunaiThrower, 'Kunai Thrower').
allName(shurikenThrower, 'Shuriken Thrower').
allName(rasengan, 'Rasengan').
allName(sexyNoJutsu, 'Sexy no Jutsu').
allName(ultimateJutsu, 'Ultimate Jutsu').
allName(scroll, 'Scroll').
allName(chakra, 'Chakra').
allName(shuriken, 'Shuriken').
allName(kunai, 'Kunai').
allName(ironArmor, 'Iron Armor').
allName(woodArmor, 'Wood Armor').
allName(nonNewtonianFluidArmor, 'Non-Newtonian Fluid Armor').

consumable(katsuyu).
consumable(healingJutsu).

heal(katsuyu, 20).
heal(healingJutsu, 10).

damage(shinobiSuna, 5).
damage(shinobiKiri, 5).
damage(shinobiIwa, 5).
damage(shinobiKumo, 5).
damage(kunaiThrower, 10).
damage(shurikenThrower, 10).
damage(rasengan, 20).
damage(sexyNoJutsu, 25).
damage(ultimateJutsu, 50).
damage(none,5).

/* Start */
start :- write('Selamat datang di desa Konoha'), nl,
	write('Kamu telah terpilih menjadi seorang genin baru, di misi tingkat S kali ini, kamu harus menjadi orang terakhir yang hidup di medan pertempuran. Yang harus kamu lakukan hanya bertahan hidup!. Jika kamu berhasil, kamu akan diangkat menjadi seorang hokage!'),
	nl,nl,
	write('Perintah yang tersedia:'), nl,
	write(' start. -- mulai permainan!'),nl,
	write(' help. -- menunjukkan perintah yang tersedia!'),nl,
	write(' quit. -- keluar dari permainan'),nl,
	write(' look. -- melihat disekitar kamu'),nl,
	write(' n. s. e. w. -- gerak'),nl,
	write(' map. -- melihat map dan mendeteksi musuh'),nl,
	write(' take(Object). -- mengambil objek'),nl,
	write(' drop(Object). -- meletakkan objek'),nl,
	write(' use(Object). -- menggunakan objek'),nl,
	write(' attack. -- serang musuh yang menghalangi jalanmu'),nl,
	write(' status. -- menunjukkan status kamu'),nl,
	write(' save(Filename). -- simpan permainan'),nl,
	write(' loaddata(Filename). -- load permainan'),nl,
	nl,
	write('Objek:'),nl,
	write('W = senjata'),nl,
	write('A = armor'),nl,
	write('M = obat-obatan'),nl,
	write('O = peluru'),nl,
	write('P = pemain'),nl,
	write('E = musuh'),nl,
	write('- = dapat diakses'),nl,
	write('X = tidak dapat diakses'), nl,
	asserta(ammo(c,3)),
	asserta(ammo(k,3)),
	asserta(ammo(s1,3)),
	asserta(ammo(p,3)),
	asserta(location(self, 5,5)),
	asserta(health(self, 100)),
	asserta(inventory([])),
	asserta(isInventFull(0)),
	asserta(weapon(self, none)),
	asserta(armor(self, none, 0)),
	asserta(peta([])),
	baca_map, setPixel(5,5,'P'),

	assignNonObject,
	assignFence,
	setEnemy(3),
	setWeapon(10),
	setArmor(10),
	setAmmo(10),
	setMedicine(10),
	asserta(nEnemy(3)),
	assignDead(1),
	assignDead(10),

	retract(game(_)),
	asserta(game(1)), !.

assignNonObject :- forall(between(2,9,Y),
   forall(between(2,9,X), assigning(X,Y)
)), !.

assigning(X, Y) :- \+(location(_,X,Y)), asserta(location(none,X,Y)), !.
assigning(X, Y) :- location(self,X,Y), asserta(location(none,X,Y)), !.
assigning(_,_).

assignFence :- forall(between(1,10,Y), (assigningFence(1,Y), assigningFence(10,Y))),
	forall(between(1,10,X), (assigningFence(X,1), assigningFence(X,10))).

assigningFence(X,Y) :- asserta(location(fence,X,Y)), !.
assigningFence(_, _).

assignDead(X) :- forall(between(1,10,Y), (asserta(location(deadzone,Y,X)),asserta(location(deadzone,X,Y)), setPixel(X,Y,'X'), setPixel(Y,X,'X'), delxp(X,Y), delxp(Y,X))).
assignDead(X) :- location(_,_,_).
delxp(X, Y) :- location(A, X, Y), A \= fence, A \= deadzone, retract(location(A,X,Y)).
delxp(_, _) :- location(_,_,_).

change([_|Tail],[C|Tail],C,0) :- !.
change([A|Tail],[A|LBaru],C,Indeks) :- IndeksBaru is Indeks-1, change(Tail,LBaru,C,IndeksBaru).

setPixel(X,Y,C) :- retract(peta(L)),
	Pos is ((Y-1)*11+X-1),
	change(L,LBaru,C,Pos),
	asserta(peta(LBaru)).

/* Weapon */
setWeapon(0) :- !.
setWeapon(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 5, U), mappingDropWeapon(U, X, Y),
	M is N-1, setWeapon(M), !.

mappingDropWeapon(1, X, Y) :- asserta(location(shurikenThrower, X, Y)), !.
mappingDropWeapon(2, X, Y) :- asserta(location(kunaiThrower, X, Y)), !.
mappingDropWeapon(3, X, Y) :- asserta(location(rasengan, X, Y)), !.
mappingDropWeapon(4, X, Y) :- asserta(location(sexyNoJutsu, X, Y)), !.
mappingDropWeapon(5, X, Y) :- asserta(location(ultimateJutsu, X, Y)), !.

/* Armor */
setArmor(0) :- !.
setArmor(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 3, U), mappingArmor(U, X, Y),
	M is N-1, setArmor(M), !.

mappingArmor(1, X, Y) :- asserta(location(nonNewtonianFluidArmor, X, Y)), !.
mappingArmor(2, X, Y) :- asserta(location(ironArmor, X, Y)), !.
mappingArmor(3, X, Y) :- asserta(location(woodArmor, X, Y)), !.

/* Medicine */
setMedicine(0) :- !.
setMedicine(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 2, U), mappingMedicine(U, X, Y),
	M is N-1, setMedicine(M), !.

mappingMedicine(1, X, Y) :- asserta(location(katsuyu, X, Y)), !.
mappingMedicine(2, X, Y) :- asserta(location(healingJutsu, X, Y)), !.

/* Ammo */
setAmmo(0) :- !.
setAmmo(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 4, U), mappingAmmo(U, X, Y),
	M is N-1, setAmmo(M), !.

mappingAmmo(1, X, Y) :- asserta(location(kunai, X, Y)), !.
mappingAmmo(2, X, Y) :- asserta(location(shuriken, X, Y)), !.
mappingAmmo(3, X, Y) :- asserta(location(chakra, X, Y)), !.
mappingAmmo(4, X, Y) :- asserta(location(scroll, X, Y)), !.

/* Enemy */
setEnemy(0) :- !.
setEnemy(N) :- random(2, 9, X), random(2, 9, Y),
  random(1, 5, E), mappingEnemy(E, X, Y), location(U, X, Y),
  random(1, 5, W), mappingWeapon(U, W), M is N-1, setEnemy(M), !.

mappingEnemy(1, X, Y) :- asserta(location(shinobiSuna, X, Y)), !.
mappingEnemy(2, X, Y) :- asserta(location(shinobiKiri, X, Y)), !.
mappingEnemy(3, X, Y) :- asserta(location(shinobiIwa, X, Y)), !.
mappingEnemy(4, X, Y) :- asserta(location(shinobiKumo, X, Y)), !.

mappingWeapon(X, 1) :- asserta(weapon(X, kunaiThrower)), !.
mappingWeapon(X, 2) :- asserta(weapon(X, shurikenThrower)), !.
mappingWeapon(X, 3) :- asserta(weapon(X, rasengan)), !.
mappingWeapon(X, 4) :- asserta(weapon(X, sexyNoJutsu)), !.

enemyMovement(X, K, L) :- random(1, 3, A), mappingEnemyMovement(X, A, K, L), !.

mappingEnemyMovement(_, 1, _, _) :- !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A > C, B > D,
  Cnew is C+1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A > C, B == D,
  Cnew is C+1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A > C, B < D,
  Cnew is C+1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A == C, B > D,
  Cnew is C, Dnew is D+1, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A == C, B == D,
  Cnew is C, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A == C, B < D,
  Cnew is C, Dnew is D-1, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A < C, B > D,
  Cnew is C-1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A < C, B == D,
  Cnew is C-1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.
mappingEnemyMovement(X, _, C, D) :-  location(self, A, B),  A < C, B < D,
  Cnew is C-1, Dnew is D, retract(location(X, C, D)), asserta(location(X, Cnew, Dnew)), !.

/* attackPlayer(X) :- location(X, A, B), location(self, A, B), retract(health(self, H)), weapon(X, W),
  damage(X, D), HP is H-D, asserta(health(self, HP)), write(X), write(' baru saja menyerangmu dengan '),
  write(W), write('. Darahmu berkurang sebanyak '), write(D). */

attackPlayer(_).

enemiesTurn :- forall(enemy(X), forall(location(X, A, B), (enemyMovement(X, A, B), attackPlayer(X)))), !.

countEnemies :- retract(nEnemy(_)), asserta(nEnemy(0)), forall(enemy(X),
	forall(location(X, _, _), (retract(nEnemy(Q)), Qnew is Q+1, asserta(nEnemy(Qnew))))), !.

/* Win Condition */
isWin :- countEnemies, nEnemy(0), retract(game(_)), asserta(game(0)),
	write('Selamat bapak Ericko Lim, Anda telah menyelesaikan misi tingat S ini!'), nl,
	write('Anda telah diangat oleh masyarakat desa Konoha menjadi Hokage ke-8'), nl,
	write('#2019GantiHokage').

/* Help */
help :- write('Perintah yang tersedia:'), nl,
	write(' start. -- mulai permainan!'),nl,
	write(' help. -- menunjukkan perintah yang tersedia!'),nl,
	write(' quit. -- keluar dari permainan'),nl,
	write(' look. -- melihat disekitar kamu'),nl,
	write(' n. s. e. w. -- gerak'),nl,
	write(' map. -- melihat map dan mendeteksi musuh'),nl,
	write(' take(Object). -- mengambil objek'),nl,
	write(' drop(Object). -- meletakkan objek'),nl,
	write(' use(Object). -- menggunakan objek'),nl,
	write(' attack. -- serang musuh yang menghalangi jalanmu'),nl,
	write(' status. -- menunjukkan status kamu'),nl,
	write(' save(Filename). -- simpan permainan'),nl,
	write(' loaddata(Filename). -- load permainan'),nl.

/* Quit */
quit :- write('Credits :'), nl,
	write('---------- Kelompok Prolog > Analog ----------'), nl,
	write('13517025 Ricky Yuliawan'), nl,
	write('13517073 Rayza Mahendra Guntara Harsono'), nl,
	write('13517145 Muhammad Al Terra'), nl,
	write('13517151 Rakhmad Budiono'), nl,
	write('write (skidipapap.) to halt'), nl.

skidipapap :- halt.

/* Look */
look :- game(0), write('Kamu belum memulai permainan'), !.
look :-
	location(self, X,Y),
	XMin is X-1,
	XMax is X+1,
	YMin is Y-1,
	YMax is Y+1,
	forall(between(YMin,YMax,J), (
		forall(between(XMin,XMax,I), (
			printMap(I,J)
		)),
		nl
	)), nl, lookEveryObject,
	!.

lookEveryObject :- checkingAround1.

checkingAround1 :- location(self, X, Y), location(Q, X, Y), enemy(Q), write('ADA ('), write(Q), write(') DI DEPANMU! '), nl, fail.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), weaponList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, fail.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), armorList(Q, _), write('Ada ('), write(Q), write(') di tanah! '), nl, fail.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), medicineList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, fail.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), ammoList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, fail.
checkingAround1 :- location(self, X, Y), location(none, X, Y), write('Kamu berada di tanah kosong. '), nl, !.
checkingAround1 :- location(_, _, _).

printMap(_,Y) :- Y == 1, write('X'), !.
printMap(_,Y) :- Y == 10, write('X'), !.
printMap(X,_) :- X == 1, write('X'), !.
printMap(X,_) :- X == 10, write('X'), !.
printMap(X,Y) :- location(Q, X, Y), enemy(Q), write('E'), !.
printMap(X,Y) :- location(Q, X, Y), weaponList(Q), write('S'), !.
printMap(X,Y) :- location(Q, X, Y), armorList(Q, _), write('A'), !.
printMap(X,Y) :- location(Q, X, Y), medicineList(Q), write('O'), !.
printMap(X,Y) :- location(Q, X, Y), ammoList(Q), write('M'), !.
printMap(X,Y) :- location(self, X, Y), write('P'), !.
printMap(_,_) :- write('-').

/* Map */
map :-  updateMap, peta(X), printList(X), !.

updateMap :- forall(between(2,9,Y),
   forall(between(2,9,X), (
      location(Q,X,Y), setMap(Q,X,Y)
   )
)), location(self, X, Y), setPixel(X,Y,'P'), !.

setMap(_, X, Y) :- \+location(deadzone, X, Y), setPixel(X, Y, '-').
setMap(_, X, Y) :- location(deadzone, X, Y), setPixel(X, Y, 'X').
setMap(_, _, _) :- location(_,_,_).
/* Buat baca data dari peta.txt */
readData(S,[]) :- at_end_of_stream(S), !.

readData(S,[X1|Tail]) :- get_char(S,X1),
	readData(S,Tail).

baca_map :- retract(peta(_)),
	open('peta.txt',read,S),
	repeat,
	readData(S,X), !,
	close(S),
	asserta(peta(X)).
/*-----------------------------*/

/* Buat nulis data ke peta.txt */
writeData(_,[]) :- !.
writeData(S,[X1|Tail]) :- write(S,X1),
	writeData(S,Tail).

write_map(L) :- open('peta.txt',write,S),
	repeat,
	writeData(S,L),
	close(S).

/* Print Map */
printList([]) :- !.
printList([A|Tail]) :- write(A), printList(Tail).

/* Move */
lookcnt :- count(X), write(X), dead(Y), nl, write(Y), nl, writedead.
writedead :- forall(between(1,10,X),(forall(between(1,10,Y), writeactual(X, Y)))).
writeactual(X, Y) :- location(deadzone,X,Y), write(X), write(' '), write(Y), nl.
writeactual(_, _) :- location(_,_,_).

n :- isWin, !.
n :- location(self, _,B), B == 2, write('Selamat, anda menabrak pagar!'), !.
n :- retract(location(self, A,B)), C is B-1, asserta(location(self, A,C)), assigning(A, B), checkingAround,
	retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), droprandom, deadzoneTrav, deadzoneCheck, updateMap, enemiesTurn, !.

s :- isWin, !.
s :- location(self, _,B), B == 9, write('Selamat, anda menabrak pagar!'), !.
s :- retract(location(self, A,B)), C is B+1, asserta(location(self, A,C)), assigning(A, B), checkingAround,
	retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), droprandom, deadzoneTrav, deadzoneCheck, updateMap, enemiesTurn, !.

w :- isWin, !.
w :- location(self, A,_), A == 2, write('Selamat, anda menabrak pagar!'), !.
w :- retract(location(self, A,B)), C is A-1, asserta(location(self, C,B)), assigning(A, B), checkingAround,
	retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), droprandom, deadzoneTrav, deadzoneCheck, updateMap, enemiesTurn, !.

e :- isWin, !.
e :- location(self, A,_), A == 9, write('Selamat, anda menabrak pagar!'), !.
e :- retract(location(self, A,B)), C is A+1, asserta(location(self, C,B)), assigning(A, B), checkingAround,
	retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), droprandom, deadzoneTrav,  deadzoneCheck, updateMap, enemiesTurn, !.

deadzoneTrav :- count(Cnt), Cnt == 10, retract(dead(DCnt)), DCntNew is DCnt + 1, asserta(dead(DCntNew)), retract(count(_)), asserta(count(1)), deadzoneDraw, !.
deadzoneTrav :- location(_,_,_).
deadzoneDraw :- write('Deadzone mendekat'), dead(X), Xnew is X, assignDead(Xnew), Dimp is 11-Xnew, assignDead(Dimp).
deadzoneDraw :- location(_,_,_).
deadzoneCheck :- location(self, X, Y), location(deadzone, X, Y), retract(health(self, Z)), Znew is Z - 10,write('Jalan-jalan di deadzone, hp-10'),nl, asserta(health(self, Znew)), death, !.
deadzoneCheck :- location(_,_,_).

death :- health(self,X), X =< 0, killPlayer.
death :- location(_,_,_).
droprandom :- count(Cnt), Cnt == 9, random(1, 10, X), forall(between(2, X, Z), (random(1,4,Sup), supplydrop(Sup))).
droprandom :- location(_,_,_).
supplydrop(Z) :- Z == 1, random(2, 9, X), random(2, 9, Y), asserta(location(katsuyu, X, Y)), write('ErickoLiem : winner winner chicken dinner! (katsuyu has been dropped at '), write(X), write(','), write(Y), write(')'), nl.
supplydrop(Z) :- Z == 2, random(2, 9, X), random(2, 9, Y), asserta(location(rasengan, X, Y)), write('ErickoLiem : winner winner chicken dinner! (rasengan has been dropped at '), write(X), write(','), write(Y), write(')'), nl.
supplydrop(Z) :- Z == 3, random(2, 9, X), random(2, 9, Y), asserta(location(kunaiThrower, X, Y)), write('ErickoLiem : winner winner chicken dinner! (kunaiThrower has been dropped at '), write(X), write(','), write(Y), write(')'), nl.
supplydrop(Z) :- Z == 4, random(2, 9, X), random(2, 9, Y), asserta(location(kunai, X, Y)), write('ErickoLiem : winner winner chicken dinner! (kunai has been dropped at '), write(X), write(','), write(Y), write(')'), nl.
killPlayer :- write('omaewa mo shindeiru'), retract(game(X)), asserta(game(0)), !.
nani :- retract(game(F)), asserta(game(1)), retract(health(self, X)), asserta(health(self, 100)), write('Reanimated'), nl.
nani :- location(_,_,_).
checkingAround :- checkingGround, checkingAround2, checkingAround3, checkingAround4, checkingAround5.

checkingGround :- location(self, X, Y), location(none, X, Y), write('Kamu berada di tanah kosong. '), nl, !.
checkingGround :- location(self, X, Y), location(_, X, Y), write('Disekitarmu ada objek, gunakan (look.) untuk melihat.'), nl, !.

checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), Q == fence, write('Ada PAGAR di sebelah barat! '), nl, !.
checkingAround2 :- !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), Q == fence, write('Ada PAGAR di sebelah timur! '), nl, !.
checkingAround3 :- !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), Q == fence, write('Ada PAGAR di sebelah utara! '), nl, !.
checkingAround4 :- !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), Q == fence, write('Ada PAGAR di sebelah selatan! '), nl, !.
checkingAround5 :- !.

/* Take */
take(_) :- game(0), write('Kau belum memulai permainan.'), !.
take(_):- isInventFull(1), write('Inventori kamu penuh.'), !.
take(A) :- location(self, X, Y), isInventFull(0), location(A, X, Y), weaponList(A), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), inventory(Q), cekInventfull(Q),!.
take(A) :- location(self, X, Y), isInventFull(0), location(A, X, Y), consumable(A), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), inventory(Q), cekInventfull(Q),!.
take(A) :- location(self, X, Y), isInventFull(0), location(A, X, Y), ammoList(A), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), inventory(Q), cekInventfull(Q), location(B,X,Y), cekEmpty(B,X,Y), !.
take(A) :- location(self, X, Y), isInventFull(0), location(A, X, Y), armorList(A,_), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), inventory(Q), cekInventfull(Q), location(B,X,Y), cekEmpty(B,X,Y), !.
take(_):- location(self,X,Y), location(none,X,Y), write('Tidak ada apapun'),!.

cekInventfull(X):-length(X,Y), Y < 5, retract(isInventFull(_)), assertz(isInventFull(0)),!.
cekInventfull(X):-length(X,5), retract(isInventFull(_)), assertz(isInventFull(1)),!.

cekEmpty(B,_,_):- weaponList(B); armorList(B,_); ammoList(B); medicineList(B),  !.
cekEmpty(_,X,Y):- asserta(location(none,X,Y)).

/* Drop */
drop(_) :- game(0), write('Kau belum memulai permainan.'), !.
drop(X) :- game(1), inventory(I), member(X, I), retract(inventory(L)), delete_one(X, L, Y), assertz(inventory(Y)), location(self, A, O), assertz(location(X, A,O)),
	write('Kau membuang '), write(X), nl,!.

dropInvent(_, [], _) :- !.
dropInvent(X, [H | T], T) :- allName(X, Z), allName(H, Z), !.
dropInvent(X, [H | T], [H | TX]) :- dropInvent(X, T, TX).

dropHand :- retract(inventory(_)), weapon(self, X), take(X),!.
dropHand :- retract(inventory(_)), weapon(self,none), !.

delete_one(_,[],[]).
delete_one(X,[X|Xs],Xs).
delete_one(X,[Y|Xs],[Y|Ys]):-X \= Y, delete_one(X,Xs,Ys).

/* Use */
use(_) :- \+ game(1), write('Kamu belum memulai permainan'), !.
use(X) :- weaponList(X), inventory(I), member(X,I), weapon(self, none), retract(weapon(self,none)),assertz(weapon(self, X)), retract(inventory(I)), delete_one(X,I,L), assertz(inventory(L)),!.
use(X) :- weaponList(X), inventory(I), member(X,I), retract(weapon(self, W)), retract(inventory(L)),
	delete_one(X, L, Y), assertz(inventory([W|Y])), assertz(weapon(self, X)),!.
use(X) :- consumable(X), inventory(I), member(X, I), retract(inventory(I)), delete_one(X, I, Y), assertz(inventory(Y)), useConsumable(X), notMoreThan100, write(X),write(' menyembuhkan anda'), !.
use(X) :- armorList(X,H), inventory(I), member(X,I),armor(self, none, _), retract(armor(self,none,_)),assertz(armor(self, X, H)),retract(inventory(I)),delete_one(X,I,L), assertz(inventory(L)),!.
use(X) :- armorList(X,H), inventory(I), member(X,I), retract(armor(self, W, _)), retract(inventory(L)),
	delete_one(X, L, Y), assertz(inventory([W|Y])), assertz(armor(self, X, H)), write('Perisai terbatas! Gunakan dengan baik!'),!.
	use(X) :- ammoList(X), inventory(I), member(X,I), retract(inventory(I)), delete_one(X, I, Y), assertz(inventory(Y)), assocAmmo(X,P), retract(ammo(P,O)), Temp is O + 5, assertz(ammo(P,Temp)), write('Sangat menyegarkan'),!.
use(_) :- write('Anda tidak punya itu').
assocAmmo(chakra,c).
assocAmmo(kunai,k).
assocAmmo(shuriken,s1).
assocAmmo(scroll,p).

useConsumable(X) :- heal(X, HHE), retract(health(self, HE)),
	CHE is HE + HHE, assertz(health(self, CHE)).

notMoreThan100 :- health(self, HE), HE >= 100, retract(health(self, _)), assertz(health(self, 100)), !.
notMoreThan100 :- health(self,HE), HE < 100,!.

/* Attack */
attack :- \+ game(1), isWin, !.
attack :- \+ game(1), write('Kamu belum memulai permainan'), !.
attack :- enemy(E), location(self,X,Y), location(E,X,Y), weapon(self,none), write('Anda tidak punya senjata, jadi anda meninjunya!'), nl, attackR(E), !.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), W == 0, write('Ammomu kosong!'), nl, retract(weapon(self,Q)), asserta(weapon(self,none)), attackR(E), retract(weapon(self,none)), asserta(weapon(self,Q)),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), health(E, H), H == 0, retract(ammo(R,W)), W1 is W-1, assertz(ammo(R,W1)),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  ammo(R,W), attackR(E), retract(ammo(R,W)), W1 is W-1, assertz(ammo(R,W1)), !.
attack :- write('Anda menyerang angin!'),!.

ammospec(kunaiThrower,k).
ammospec(shurikenThrower,s1).
ammospec(rasengan,c).
ammospec(sexyNoJutsu,p).
ammospec(ultimateJutsu,c).

/* Status */
status :- game(0), write('Kau belum memulai permainan.'), fail, !.
status :- game(1), health(self, HE), write('Darah : '), write(HE), nl,
	inventory(L), weapon(self,B), write('Senjata : '), write(B), nl, armor(self,W,H), write('Armor: '), write(W), nl, write('Proteksi: '), write(H), nl,
	write('Inventori : '), writestatinv(L), nl, write('Cakra: '), ammo(c,T1), write(T1), nl, write('Kunai: '), ammo(k,T2), write(T2), nl, write('Shuriken: '), ammo(s1,T3), write(T3), nl,  write('Scroll: '), ammo(p,T4), write(T4),!.


writestatinv(L):- inventory(L), L == [], write('Anda tidak punya apa-apa'),!.
writestatinv(L):- write(L),!.

attackR(X) :- game(1), enemy(X), location(X, A, O), location(self, A, O),
	retract(health(X, H)), retract(health(self, HS)), weapon(self,P), armor(self, Temp1, Q),
	damage(P, DS),  damage(X, DE), SCH is HS-DE+(Q*0.1), ECH is H-DS,
	assertz(health(X, ECH)), assertz(health(self, SCH)),
	write('You attack the '), write(X), write(' with a '), write(P), nl,
	Temp is Q-DE, retract(armor(self, Temp1, Q)), asserta(armor(self, Temp1, Temp)), validasiArmor,
	write('You damaged the enemy by '), write(DS), write(' points'), nl,
	write('The '), write(X), write(' damaged you by '), write(DE), write(' points'), nl,
	SCH > 0,
	write('Your health is '), write(SCH), nl,
	ECH > 0,
	write('The '), write(X), write(' health is '), write(ECH), nl,!.

attackR(X) :- retract(health(X, H)), H =< 0, asserta(health(X,0)), location(self,T,T1), retract(location(X, T, T1)), weapon(X,P), asserta(location(P,T,T1)),
	write('The '), write(X), write(' is dead!'), nl,  write('The enemy drops a '), write(P), retract(health(X, H)), asserta(health(X,50)),!.

attackR(_) :- health(self, SCH), SCH =< 0, write('You are dead. Better be ready next time.'), nl, retract(game(_)), assertz(game(0)).

validasiArmor :- armor(self, _, Q), Q  > 0, !.
validasiArmor :- armor(self, A, Q), Q == 0, retract(armor(self,A,Q)), asserta(armor(self,none,0)),!.
validasiArmor :- armor(self, A, Q), Q < 0, retract(armor(self,A,Q)), asserta(armor(self,none,0)),!.

/* Save */
save(Name) :-
	open(Name,write,Savedata),
	health(self,HE),
	weapon(self,WE),
	armor(self,AR, AQ),
	location(self,X,Y),
	write(Savedata,HE),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,WE),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,AR),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,AQ),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,X),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,Y),	write(Savedata,'.'),nl(Savedata),
	write('Data sudah tersimpan!'),nl,
	close(Savedata).

 /* Load */
 	loaddata(Name) :-
	open(Name,read,Savedata),
	health(self,HE),
	weapon(self,WE),
	armor(self,AR, AQ),
	location(self,X,Y),
	retract(health(self,HE)),
	retract(weapon(self,WE)),
	retract(armor(self,AR, AQ)),
	retract(location(self,X,Y)),
	read(Savedata,NewHE),
	read(Savedata,NewWE),
	read(Savedata,NewAR),
	read(Savedata,NewAQ),
	read(Savedata,NewX),
	read(Savedata,NewY),
	write('membaca save data berhasil...'),nl,
	asserta(health(self,NewHE)),
	asserta(weapon(self,NewWE)),
	asserta(armor(self,NewAR, NewAQ)),
	asserta(location(self,NewX,NewY)),
	write('Load save data berhasil!'),nl,
	close(Savedata).

/* CHEAT */

/* HP = 9999 */
baguvix :-	retract(health(self,_)),
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

aezakmi :-	forall(between(2,9,Y),
		forall(between(2,9,X),
		(location(Q,X,Y), setMapCheat(Q,X,Y)))),
		location(self, X, Y), setPixel(X,Y,'P'),delDeadzone, !,
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
