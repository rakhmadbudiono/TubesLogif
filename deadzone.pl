isDeadzone(X,Y) :- 	deadzone(DZ), lebarPeta(L), tinggiPeta(T), 
					NotDZXMin is DZ + 1, NotDZYMin is DZ + 1,
					NotDZXMax is L - DZ, NotDZYMax is T - DZ,
					(Y < NotDZYMin, Y > NotDZYMax, X < NotDZXMin, X > NotDZXMax), !.
					
incDeadzone :- retract(deadzone(DZ)), DZBaru is DZ + 1, asserta(deadzone(DZBaru)),!.

gambarDeadzone(0) :- 1.
gambarDeadzone(DZ) :- 	lebarPeta(Le), tinggiPeta(Ti),
						XMin is DZ, XMax is Le - DZ + 1,
						YMin is DZ, YMax is Ti - DZ + 1,
						forall(between(XMin,XMax,X),(setPixel(X,YMin,'X'),setPixel(X,YMax,'X'))),
						forall(between(YMin,YMax,Y),(setPixel(XMin,Y,'X'),setPixel(XMax,Y,'X'))),
						DZBaru is DZ - 1, gambarDeadzone(DZBaru),!.
