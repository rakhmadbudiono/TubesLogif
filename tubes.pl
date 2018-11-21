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

armorList(ironArmor).
armorList(woodArmor).
armorList(nonNewtonianFluidArmor).

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

	asserta(location(self, 3,3)),
	asserta(health(self, 100)),
	asserta(inventory([])),
	asserta(weapon(self, none)),
	asserta(armor(self, none)),
	asserta(peta([])),
	baca_map, setPixel(3,3,'P'),

	asserta(game(1)).

change([_|Tail],[C|Tail],C,0) :- !.
change([A|Tail],[A|LBaru],C,Indeks) :- IndeksBaru is Indeks-1, change(Tail,LBaru,C,IndeksBaru).

setPixel(X,Y,C) :- retract(peta(L)),
	Pos is ((Y-1)*6+X-1),
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
look :- look1, look2, look3, look4, look5, look6.

look1 :- game(1), location(self, X, Y), N is Y+1,
	location(A, X, N), write('Ada '), allName(A, _), write(A), write(' disebelah selatan'), nl, !.
look1 :- write('Di sebelah selatan tidak ada objek'), nl, !.
look2 :- game(1), location(self, X, Y), S is Y-1,
	location(A, X, S), write('Ada '), allName(A, _), write(A), write(' disebelah utara'), nl, !.
look2 :- write('Di sebelah utara tidak ada objek'), nl, !.
look3 :- game(1), location(self, X, Y), W is X-1,
	location(A, W, Y), write('Ada '), allName(A, _), write(A), write(' disebelah barat'), nl, !.
look3 :- write('Di sebelah barat tidak ada objek'),nl, !.
look4 :- game(1), location(self, X, Y), E is X+1,
	location(A, E, Y), write('Ada '), allName(A, _), write(A), write(' disebelah timur'),  nl, !.
look4 :- write('Di sebelah timur tidak ada objek'), nl, !.
look5 :- game(1), location(self, X, Y), location(A, X, Y), A \== self, \+ enemy(A), allName(A, _), write('Ada '),
	write(A), write(' di tanah'), nl, !.
look5 :- write('Di tanah tidak ada objek'), nl, !.
look6 :- game(1), location(self, X, Y), location(A, X, Y), A \== self, enemy(A), allName(A, _), write('ADA '),
	write(A), write(' DIBELAKANGMU!'), nl, !.
look6 :- !.

/* Map */
map :- retract(peta(X)), printList(X), asserta(peta(X)), !.

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
n :- retract(location(self, A,B)), setPixel(A, B, '-'), C is B-1, setPixel(A, C, 'P'), asserta(location(self, A,C)), !.

s :- location(self, _,B), B == 4, write('Selamat, anda menabrak pagar!'), !.
s :- location(self, A,B), setPixel(A, B, '-'), C is B+1, setPixel(A, C, 'P'), asserta(location(self, A,C)), !.

w :- location(self, A,_), A == 2, write('Selamat, anda menabrak pagar!'), !.
w :- location(self, A,B), setPixel(A, B, '-'), C is A-1, setPixel(C, B, 'P'), asserta(location(self, C,B)), !.

e :- location(self, A,_), A == 4, write('Selamat, anda menabrak pagar!'), !.
e :- location(self, A,B), setPixel(A, B, '-'), C is A+1, setPixel(C, B, 'P'), asserta(location(self, C,B)), !.

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
save(X) :- game(1), atom_concat(X, '.pl', Z), tell(Z), write(':- dynamic(health/2).\n\
	:- dynamic(enemy/1).\n\
	:- dynamic(damage/2).\n\
	:- dynamic(moveChance/2).\n\
	:- dynamic(location/3).\n\
	:- dynamic(mapSize/2).\n\
	:- dynamic(object/1).\n\
	:- dynamic(allName/2).\n\
	:- dynamic(consumable/1).\n\
	:- dynamic(weapon/1).\n\
	:- dynamic(heal/4).\n\
	:- dynamic(durability/2).\n\
	:- dynamic(thirst/1).\n\
	:- dynamic(hunger/1).\n\
	:- dynamic(game/1).\n\
	:- dynamic(inventory/1).\n\
	:- dynamic(prev/1).\n\
	:- dynamic(equipment/1).\n\
	:- dynamic(lake/1).\n\
	:- dynamic(tree/1).\n'), listing(enemy), listing(location), listing(durability),
		listing(heal), listing(allName), listing(damage), listing(moveChance), listing(durability), listing(hunger),
		listing(thirst), listing(inventory), listing(lake), listing(game), listing(equipment),
		listing(consumable), listing(mapSize), listing(weapon), listing(prev), listing(health), listing(lake), listing(tree), told.

/* Load */
