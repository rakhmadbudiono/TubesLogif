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
dead(1).
count(0).

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
	assignDead(1),
	assignDead(10),

	asserta(game(1)).

assignNonObject :- forall(between(2,9,Y),
   forall(between(2,9,X), assigning(X,Y)
)), !.

assigning(X, Y) :- not(location(_,X,Y)), asserta(location(none,X,Y)), !.
assigning(X, Y) :- location(self,X,Y), asserta(location(none,X,Y)), !.
assigning(_,_).

assignFence :- forall(between(1,10,Y), (assigningFence(1,Y), assigningFence(10,Y))),
	forall(between(1,10,X), (assigningFence(X,1), assigningFence(X,10))).

assigningFence(X,Y) :- asserta(location(fence,X,Y)), !.
assigningFence(_, _).

assignDead(X) :- forall(between(1,10,Y), (asserta(location(deadzone,Y,X)),asserta(location(deadzone,X,Y)), setPixel(X,Y,'X'), setPixel(Y,X,'X'), delxp(X,Y), delxp(Y,X))).
delxp(X, Y) :- location(A, X, Y), A \= fence, A \= deadzone, retract(location(A,X,Y)).
delxp(X, Y) :- location(_,_,_).

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
writeactual(X, Y) :- location(_,_,_).

n :- location(self, _,B), B == 2, write('Selamat, anda menabrak pagar!'), !.
n :- retract(location(self, A,B)), C is B-1, asserta(location(self, A,C)), assigning(A, B), checkingAround, retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), deadzoneTrav, deadzoneCheck, updateMap,!.

s :- location(self, _,B), B == 9, write('Selamat, anda menabrak pagar!'), !.
s :- retract(location(self, A,B)), C is B+1, asserta(location(self, A,C)), assigning(A, B), checkingAround, retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), deadzoneTrav, deadzoneCheck, updateMap,!.

w :- location(self, A,_), A == 2, write('Selamat, anda menabrak pagar!'), !.
w :- retract(location(self, A,B)), C is A-1, asserta(location(self, C,B)), assigning(A, B), checkingAround, retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)), deadzoneTrav, deadzoneCheck, updateMap,!.

e :- location(self, A,_), A == 9, write('Selamat, anda menabrak pagar!'), !.
e :- retract(location(self, A,B)), C is A+1, asserta(location(self, C,B)), assigning(A, B), checkingAround, retract(count(Cnt)), Dmp is Cnt + 1, asserta(count(Dmp)),deadzoneTrav,  deadzoneCheck, updateMap,!.

deadzoneTrav :- count(Cnt), Cnt == 10, retract(dead(DCnt)), DCntNew is DCnt + 1, asserta(dead(DCntNew)), retract(count(DUMP)), asserta(count(1)), deadzoneDraw, !.
deadzoneTrav :- location(_,_,_).
deadzoneDraw :- write('Deadzone mendekat'), dead(X), Xnew is X, assignDead(Xnew), Dimp is 11-Xnew, assignDead(Dimp).
deadzoneDraw :- location(_,_,_).
deadzoneCheck :- location(self, X, Y), location(deadzone, X, Y), killPlayer, !.
deadzoneCheck :- location(_,_,_).

killPlayer :- write('ANDA MATI CYKA BLYAT'), !.

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
take(_):- location(self,X,Y), location(none,X,Y), write("Tidak ada apapun"),!.

cekInventfull(X):-length(X,Y), Y < 5, retract(isInventFull(_)), assertz(isInventFull(0)),!.
cekInventfull(X):-length(X,5), retract(isInventFull(_)), assertz(isInventFull(1)),!.

cekEmpty(B,_,_):- weaponList(B); armorList(B,_); ammoList(B); medicineList(B),  !.
cekEmpty(B,X,Y):- asserta(location(none,X,Y)).

/* Drop */
drop(_) :- game(0), write('Kau belum memulai permainan.'), !.
drop(X) :- game(1), inventory(I), member(X, I), retract(inventory(L)), delete_one(X, L, Y), assertz(inventory(Y)), location(self, A, O), assertz(location(X, A,O)),
	write('Kau membuang '), write(X), nl,!.

dropInvent(_, [], _) :- !.
dropInvent(X, [H | T], T) :- allName(X, Z), allName(H, Z), !.
dropInvent(X, [H | T], [H | TX]) :- dropInvent(X, T, TX).

dropHand :- retract(inventory(L)), weapon(self, X), take(X),!.
dropHand :- retract(inventory(L)), weapon(self,none), !.

delete_one(X,[],[]).
delete_one(X,[X|Xs],Xs).
delete_one(X,[Y|Xs],[Y|Ys]):-X \= Y, delete_one(X,Xs,Ys).

/* Use */
use(_) :- \+ game(1), write('Kamu belum memulai permainan'), !.
use(X) :- weaponList(X), inventory(I), member(X,I), weapon(self, none), retract(weapon(self,none)),assertz(weapon(self, X)), retract(inventory(I)), delete_one(X,I,L), assertz(inventory(L)),!.
use(X) :- weaponList(X), inventory(I), member(X,I), retract(weapon(self, W)), retract(inventory(L)),
	delete_one(X, L, Y), assertz(inventory([W|Y])), assertz(weapon(self, X)),!.
use(X) :- consumable(X), inventory(I), member(X, I), retract(inventory(L)), delete_one(X, L, Y), assertz(inventory(Y)), useConsumable(X), notMoreThan100, !.
use(X) :- armorList(X,H), inventory(I), member(X,I),armor(self, none, _), retract(armor(self,none,_)),assertz(armor(self, X, H)),retract(inventory(I)),delete_one(X,I,L), assertz(inventory(L)),!.
use(X) :- armorList(X,H), inventory(I), member(X,I), retract(armor(self, W, _)), retract(inventory(L)),
	delete_one(X, L, Y), assertz(inventory([W|Y])), assertz(armor(self, X, H)),!.
use(X) :- write('Anda tidak punya itu').

useConsumable(X) :- heal(X, HHE), retract(health(self, HE)),
	CHE is HE + HHE, assertz(health(self, CHE)).

notMoreThan100 :- health(self, HE), HE > 100, retract(health(self, _)), assertz(health(self, 100)), !.

/* Attack */
attack :- \+ game(1), write('Kamu belum memulai permainan'), !.
attack :- weapon(self,none), write("Anda tidak punya senjata."),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  \+member(R,L), write("Ammomu kosong!"),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  member(R,L), health(E, H), H == 0, retract(inventory(L)), delete_one(R,L,T), assertz(inventory(T)),!.
attack :- enemy(E), location(self, X, Y), location(E, X, Y), inventory(L), weapon(self,Q), ammospec(Q,R),  member(R,L), attackR(E), retract(inventory(L)), delete_one(R,L,T), assertz(inventory(T)), !.
attack :- write("Anda menyerang angin!"),!.
ammospec(kunaiThrower,kunai).
ammospec(shurikenThrower,shuriken).
ammospec(rasengan,chakra).
ammospec(sexyNoJutsu,scroll).
ammospec(ultimateJutsu,chakra).
/* Status */
status :- game(0), write('Kau belum memulai permainan.'), fail, !.
status :- game(1), health(self, HE), write('Darah : '), write(HE), nl,
	inventory(L), weapon(self,B), write('Senjata : '), write(B), nl, armor(self,W,H), write('Armor: '), write(W), nl, write('Kondisi: '), write(H), nl,
	write('Inventori : '), writestatinv(L),!.

writestatinv(L):- inventory(L), L == [], write('Anda tidak punya apa-apa'),!.
writestatinv(L):- write(L),!.

attackR(X) :- game(1), enemy(X), location(X, A, O), location(self, A, O),
	retract(health(X, H)), retract(health(self, HS)), weapon(self,P), armor(self, _, Q),
	damage(P, DS),  damage(X, DE), SCH is HS-DE+(Q*0.1), ECH is H-DS,
	assertz(health(X, ECH)), assertz(health(self, SCH)),
	write('You attack the '), write(X), write(' with a '), write(P), nl,
	write('You damaged the enemy by '), write(DS), write(' points'), nl,
	write('The '), write(X), write(' damaged you by '), write(DE), write(' points'), nl,
	SCH > 0,
	write('Your health is '), write(SCH), nl,
	ECH > 0,
	write('The '), write(X), write(' health is '), write(ECH), nl,!.

attackR(X) :- retract(health(X, H)), H =< 0, assertz(health(X, 0)), retract(location(X, _, _)),
	write('The '), write(X), write(' is dead!'), nl,!.

attackR(_) :- health(self, SCH), SCH =< 0, write('You are dead. Better be ready next time.'), nl, retract(game(_)), assertz(game(0)).

/* Save */
save(Name) :-
	open(Name,write,Savedata),
	health(self,HE),
	weapon(self,WE),
	armor(self,AR),
	location(self,X,Y),
	write(Savedata,HE),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,WE),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,AR),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,X),	write(Savedata,'.'),nl(Savedata),
	write(Savedata,Y),	write(Savedata,'.'),nl(Savedata),
	write('Data sudah tersimpan!'),nl,
	close(Savedata).

 /* Load */
	open(Name,read,Savedata),
	health(self,HE),
	weapon(self,WE),
	armor(self,AR),
	location(self,X,Y),
	retract(health(self,HE)),
	retract(weapon(self,WE)),
	retract(armor(self,AR)),
	retract(location(self,X,Y)),
	read(Savedata,NewHE),
	read(Savedata,NewWE),
	read(Savedata,NewAR),
	read(Savedata,NewX),
	read(Savedata,NewY),
	write('membaca save data berhasil...'),nl,
	asserta(health(self,NewHE)),
	asserta(weapon(self,NewWE)),
	asserta(armor(self,NewAR)),
	asserta(location(self,NewX,NewY)),
	write('Load save data berhasil!'),nl,
	close(Savedata).
