/* Save */
save(_) :- game(0), write('Kamu belum memulai permainan'), !.

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
loadFile(_) :- game(0), write('Kamu belum memulai permainan'), !.

loadFile(Name) :-
	open(Name,read,Savedata),
	retract(health(self,_)),
	retract(weapon(self,_)),
	retract(armor(self,_, _)),
	retract(location(self,_,_)),
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