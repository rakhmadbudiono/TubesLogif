/* Nama Kelompok : Prolog > Analog
Kelas : K-01
Nama Anggota :
  - Ricky Yuliawan 13517025
  - Rayza Mahendra Guntara Harsono 13517073
  - Muhammad Al Terra 13517145
  - Rakhmad Budiono 13517151 */

/* Fact */
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

heal(katsuyu, 0).
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

/* Start */
start :- write("Selamat datang di desa Konoha"), nl,
	write("Kamu telah terpilih menjadi seorang genin baru, di misi tingkat S kali ini, kamu harus menjadi orang terakhir yang hidup di medan pertempuran. Yang harus kamu lakukan hanya bertahan hidup!. Jika kamu berhasil, kamu akan diangkat menjadi seorang hokage!"),
	nl,nl,
	write("Perintah yang tersedia:"), nl,
	write(" start. -- mulai permainan!"),nl,
	write(" help. -- menunjukkan perintah yang tersedia!"),nl,
	write(" quit. -- keluar dari permainan"),nl,
	write(" look. -- melihat disekitar kamu"),nl,
	write(" n. s. e. w. -- gerak"),nl,
	write(" map. -- melihat map dan mendeteksi musuh"),nl,
	write(" take(Object). -- mengambil objek"),nl,
	write(" drop(Object). -- meletakkan objek"),nl,
	write(" use(Object). -- menggunakan objek"),nl,
	write(" attack. -- serang musuh yang menghalangi jalanmu"),nl,
	write(" status. -- menunjukkan status kamu"),nl,
	write(" save(Filename). -- simpan permainan"),nl,
	write(" load(Filename). -- load permainan"),nl,
	nl,
	write("Objek:"),nl,
	write("W = senjata"),nl,
	write("A = armor"),nl,
	write("M = obat-obatan"),nl,
	write("O = peluru"),nl,
	write("P = pemain"),nl,
	write("E = musuh"),nl,
	write("- = dapat diakses"),nl,
	write("X = tidak dapat diakses"), nl,

	asserta(location(self, 5,5)),
	asserta(health(self, 100)),
	asserta(inventory([])),
	asserta(weapon(self, none)),
	asserta(armor(self, none, 0)),
	asserta(peta([])),
	baca_map, setPixel(5,5,'P'),

	asserta(location(kunaiThrower,2,3)),
	asserta(location(ironArmor,2,3)),
	asserta(location(kunai,3,2)),
	asserta(location(katsuyu,3,4)),
	asserta(location(shinobiIwa,2,2)),

	assignNonObject,
	assignFence,

	asserta(game(1)).

assignNonObject :- forall(between(2,9,Y),
   forall(between(2,9,X), assigning(X,Y)
)), !.

assigning(X, Y) :- not(location(_,X,Y)), asserta(location(none,X,Y)), !.
assigning(_,_).

assignFence :- forall(between(1,10,Y), (assigningFence(1,Y), assigningFence(10,Y))),
	forall(between(1,10,X), (assigningFence(X,1), assigningFence(X,10))).

assigningFence(X,Y) :- asserta(location(fence,X,Y)), !.
assigningFence(_, _).

change([_|Tail],[C|Tail],C,0) :- !.
change([A|Tail],[A|LBaru],C,Indeks) :- IndeksBaru is Indeks-1, change(Tail,LBaru,C,IndeksBaru).

setPixel(X,Y,C) :- retract(peta(L)),
	Pos is ((Y-1)*11+X-1),
	change(L,LBaru,C,Pos),
	asserta(peta(LBaru)).

/* Help */
help :- write("Perintah yang tersedia:"), nl,
	write(" start. -- mulai permainan!"),nl,
	write(" help. -- menunjukkan perintah yang tersedia!"),nl,
	write(" quit. -- keluar dari permainan"),nl,
	write(" look. -- melihat disekitar kamu"),nl,
	write(" n. s. e. w. -- gerak"),nl,
	write(" map. -- melihat map dan mendeteksi musuh"),nl,
	write(" take(Object). -- mengambil objek"),nl,
	write(" drop(Object). -- meletakkan objek"),nl,
	write(" use(Object). -- menggunakan objek"),nl,
	write(" attack. -- serang musuh yang menghalangi jalanmu"),nl,
	write(" status. -- menunjukkan status kamu"),nl,
	write(" save(Filename). -- simpan permainan"),nl,
	write(" load(Filename). -- load permainan"),nl.

/* Quit */
quit :- write("Credits :"), nl,
	write("---------- Kelompok Prolog > Analog ----------"), nl,
	write("13517025 Ricky Yuliawan"), nl,
	write("13517073 Rayza Mahendra Guntara Harsono"), nl,
	write("13517145 Muhammad Al Terra"), nl,
	write("13517151 Rakhmad Budiono"), nl,
	write("write (skidipapap.) to halt"), nl.

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
	)),
	!.

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
)), !.

setMap(Q, X, Y) :- Q == self,setPixel(X,Y,'P'), !.
setMap(_, X, Y) :- setPixel(X, Y, '-').

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
n :- location(self, _,B), B == 2, write('Selamat, anda menabrak pagar!'), !.
n :- retract(location(self, A,B)), C is B-1, asserta(location(self, A,C)), assigning(A, B), updateMap, checkingAround, !.

s :- location(self, _,B), B == 9, write('Selamat, anda menabrak pagar!'), !.
s :- retract(location(self, A,B)), C is B+1, asserta(location(self, A,C)), assigning(A, B), updateMap, checkingAround, !.

w :- location(self, A,_), A == 2, write('Selamat, anda menabrak pagar!'), !.
w :- retract(location(self, A,B)), C is A-1, asserta(location(self, C,B)), assigning(A, B), updateMap, checkingAround, !.

e :- location(self, A,_), A == 9, write('Selamat, anda menabrak pagar!'), !.
e :- retract(location(self, A,B)), C is A+1, asserta(location(self, C,B)), assigning(A, B), updateMap, checkingAround, !.

checkingAround :- checkingAround1, checkingAround2, checkingAround3, checkingAround4, checkingAround5.

checkingAround1 :- location(self, X, Y), location(Q, X, Y), enemy(Q), write('ADA ('), write(Q), write(') DI DEPANMU! '), nl, !.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), weaponList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, !.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), armorList(Q, _), write('Ada ('), write(Q), write(') di tanah! '), nl, !.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), medicineList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, !.
checkingAround1 :- location(self, X, Y), location(Q, X, Y), ammoList(Q), write('Ada ('), write(Q), write(') di tanah! '), nl, !.
checkingAround1 :- location(self, X, Y), location(_, X, Y), write('Kamu berada di tanah kosong. '), nl, !.

checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), enemy(Q), write('ADA ('), write(Q), write(') DI SEBELAH BARAT! '), nl, !.
checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), weaponList(Q), write('Ada ('), write(Q), write(') di sebelah barat! '), nl, !.
checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), armorList(Q, _), write('Ada ('), write(Q), write(') di sebelah barat! '), nl, !.
checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), medicineList(Q), write('Ada ('), write(Q), write(') di sebelah barat! '), nl, !.
checkingAround2 :- location(self, X, Y), Xnew is X-1, location(Q, Xnew, Y), ammoList(Q), write('Ada ('), write(Q), write(') di sebelah barat! '), nl, !.
checkingAround2 :- location(self, X, Y), Xnew is X-1, location(_, Xnew, Y), write('Di baratmu adalah tanah kosong. '), nl, !.

checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), enemy(Q), write('ADA ('), write(Q), write(') DI SEBELAH TIMUR! '), nl, !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), weaponList(Q), write('Ada ('), write(Q), write(') di sebelah timur! '), nl, !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), armorList(Q, _), write('Ada ('), write(Q), write(') di sebelah timur! '), nl, !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), medicineList(Q), write('Ada ('), write(Q), write(') di sebelah timur! '), nl, !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(Q, Xnew, Y), ammoList(Q), write('Ada ('), write(Q), write(') di sebelah timur! '), nl, !.
checkingAround3 :- location(self, X, Y), Xnew is X+1, location(_, Xnew, Y), write('Di timurmu adalah tanah kosong. '), nl, !.

checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), enemy(Q), write('ADA ('), write(Q), write(') DI SEBELAH UTARA! '), nl, !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), weaponList(Q), write('Ada ('), write(Q), write(') di sebelah utara! '), nl, !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), armorList(Q, _), write('Ada ('), write(Q), write(') di sebelah utara! '), nl, !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), medicineList(Q), write('Ada ('), write(Q), write(') di sebelah utara! '), nl, !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(Q, X, Ynew), ammoList(Q), write('Ada ('), write(Q), write(') di sebelah utara! '), nl, !.
checkingAround4 :- location(self, X, Y), Ynew is Y-1, location(_, X, Ynew), write('Di utaramu adalah tanah kosong. '), nl, !.

checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), enemy(Q), write('ADA ('), write(Q), write(') DI SEBELAH SELATAN! '), nl, !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), weaponList(Q), write('Ada ('), write(Q), write(') di sebelah selatan! '), nl, !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), armorList(Q, _), write('Ada ('), write(Q), write(') di sebelah selatan! '), nl, !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), medicineList(Q), write('Ada ('), write(Q), write(') di sebelah selatan! '), nl, !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(Q, X, Ynew), ammoList(Q), write('Ada ('), write(Q), write(') di sebelah selatan! '), nl, !.
checkingAround5 :- location(self, X, Y), Ynew is Y+1, location(_, X, Ynew), write('Di selatanmu adalah tanah kosong. '), nl, !.

/* Take */
take(_) :- game(0), write('Kau belum memulai permainan.'), !.
take(_):- isInventFull, write('Inventori kamu penuh.'), !.
take(A) :- location(self, X, Y), location(A, X, Y), weaponList(A), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), !.
take(A) :- location(self, X, Y), location(A, X, Y), consumable(A), write('Kau mengambil '), allName(A, N), write(N), nl,
	retract(location(A,X,Y)), retract(inventory(L)), assertz(inventory([A | L])), !.

/* Drop */
drop(_) :- game(0), write('Kau belum memulai permainan.'), !.
drop(X) :- game(1), inventory(I), inInvent(X, I), retract(inventory(L)), dropInvent(X, L, Y), assertz(inventory(Y)), location(self, A, O), assertz(location(X, A,O)),
	write('Kau membuang '), write(X), nl.

dropInvent(_, [], _) :- !.
dropInvent(X, [H | T], T) :- allName(X, Z), allName(H, Z), !.
dropInvent(X, [H | T], [H | TX]) :- dropInvent(X, T, TX).

dropHand :- retract(inventory(L)), dropInvent(hand, L, Y), assertz(inventory(Y)).

/* Use */
use(_) :- \+ game(1), write('Kamu belum memulai permainan'), !.
use(X) :- weaponList(X), retract(equipment(W)), retract(inventory(L)),
	dropInvent(X, L, Y), assertz(inventory([W |Y])), dropHand, assertz(equipment(X)),!.
use(X) :- consumable(X), inventory(I), inInvent(X, I), retract(inventory(L)), dropInvent(X, L, Y), assertz(inventory(Y)), useConsumable(X), notMoreThan100, !.

useConsumable(X) :- heal(X, HHE), retract(health(self, HE)),
	CHE is HE + HHE, assertz(health(self, CHE)).

notMoreThan100 :- health(self, HE), HE > 100, retract(health(self, _)), assertz(health(self, 100)), !.

/* Attack */
attack :- \+ game(1), write('Kamu belum memulai permainan'), !.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), health(E, H), H == 0, !.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), attackR(E), !.


/* Status */
status :- game(0), write('Kau belum memulai permainan.'), fail, !.
status :- game(1), health(self, HE), write('Darah : '), write(HE), nl,
	inventory(L), write('Senjata : '), write(EQ), nl,
	equipment(EQ), write('Inventori : '), write(L), nl.

attackR(X) :- game(1), enemy(X), location(X, A, O), location(self, A, O),
	retract(health(X, H)), retract(health(self, HS)),
	damage(self, DS), equipment(EQ), damage(EQ, DQ), D is DQ+DS, damage(X, DE), SCH is HS-DE, ECH is H-D,
	assertz(health(X, ECH)), assertz(health(self, SCH)),
	write('You attack the '), write(X), write(' with a '), allName(EQ, NQ), write(NQ), nl,
	write('You damaged the enemy by '), write(D), write(' points'), nl,
	write('The '), write(X), write(' damaged you by '), write(DE), write(' points'), nl,
	SCH > 0,
	write('Your health is '), write(SCH), nl,
	ECH > 0,
	write('The '), write(X), write(' health is '), write(ECH), nl,
	attackR(X), !.

attackR(X) :- retract(health(X, H)), H =< 0, assertz(health(X, 0)), retract(location(X, _, _)),
	write('The '), write(X), write(' is dead!'), nl,
	retract(enemy(X)), !.

attackR(_) :- health(self, SCH), SCH =< 0, write('You are dead. Better be ready next time.'), nl, retract(game(_)), assertz(game(0)).

/* Save */

/* Load */
