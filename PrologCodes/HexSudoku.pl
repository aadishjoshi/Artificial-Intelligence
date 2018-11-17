%*****************************************************************************
% Sudoku 16X16
% Written By :- Aadish Joshi
%%*****************************************************************************

:- use_module(library(clpfd)). 

%*****************************************************************************
% Sudoku function
%%*****************************************************************************

sudokuHexa(Rows) :- length(Rows, 16), maplist(length_list(16), Rows), 
		append(Rows, Vs), 
		Vs ins 0..15, maplist(all_distinct, Rows), 
		transpose(Rows, Columns), 
		maplist(all_distinct, Columns),
		Rows = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], 
		blocks(A, B, C, D), blocks(E, F, G, H), blocks(I,J,K,L), blocks(M,N,O,P),
		label(Vs).

length_list(L, Ls) :- length(Ls, L). 
blocks([], [], [], []). 
blocks([A,B,C,D|Bs1], [E,F,G,H|Bs2], [I,J,K,L|Bs3], [M,N,O,P|Bs4]) :- all_distinct([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]), blocks(Bs1, Bs2, Bs3, Bs4).


%*****************************************************************************
% File Writing
%%*****************************************************************************
write_file(Rows,_F) :-	findall(Rows, sudokuHexa(Rows), Sol),
							open(_F, write, ID), write(ID, Sol), close(ID).

countnum(Rows) :- 	findall([], sudokuHexa(Rows), Result), length(Result,L),
					    write('NN: '),write(L), write(' solutions').

%*****************************************************************************
% Input Problem generate
%%*****************************************************************************		
						
createPb(Rows,X,Y,Pb):- write('Starting...'),
		X>0, Y>0,
		replace(Rows,X,Y,_,Xs),
		X1 is X-1,
		Y1 is Y-1,
		createPb(Xs,X1,Y1),
		write(Xs).

replace(L , X , Y , Z , R ) :-
	append(RowPfx,[Row|RowSfx],L),
	length(RowPfx,X),     
	append(ColPfx,[_|ColSfx],Row),
	length(ColPfx,Y),                 
	append(ColPfx,[Z|ColSfx],RowNew),
	append(RowPfx,[RowNew|RowSfx],R).

%*****************************************************************************
% Main function to handle sudoku and solution writing.
%%*****************************************************************************

sudoku(Rows, _F)  :-	write_file(Rows,_F),
			call(sudokuHexa(Rows)),			
			%call(createPb(Rows,1,1)),
			call(countnum(Rows)), nl,
			call(sudokuHexa(Rows)) -> write('Solution = '),
			write(Rows),nl;
			write("No Solutions").

%*****************************************************************************
% Asserted Problem 
%%*****************************************************************************	
problem(1, P):-
	P  =	[[_,2,6,_,_,11,_,8,_,7,5,4,10,_,_,_], 
			[4,5,_,3,_,_,10,0,9,14,_,_,7,2,_,11],
			[7,_,_,8,_,_,15,_,2,_,10,_,_,_,12,1], 
			[_,10,_,_,_,_,3,_,6,0,12,_,4,_,_,_],
			[13,_,5,_,3,_,_,_,14,_,_,_,_,_,7,12],
			[6,_,7,_,_,_,_,_,10,_,_,_,_,_,_,_],
			[_,_,_,15,_,9,_,7,1,_,_,_,13,8,_,4],
			[8,0,_,_,_,_,_,4,12,5,_,11,15,6,_,3],
			[1,_,2,0,10,_,4,6,7,_,_,_,_,_,11,5],
			[5,_,13,6,_,_,_,11,0,_,2,_,1,_,_,_],
			[_,_,_,_,_,_,_,2,_,_,_,_,_,9,_,10],
			[9,12,_,_,_,_,_,14,_,_,_,6,_,15,_,2],
			[_,_,_,7,_,10,13,5,_,9,_,_,_,_,3,_],
			[14,6,_,_,_,4,_,3,_,2,_,_,11,_,_,7],
			[11,_,8,4,_,_,6,1,3,12,_,_,5,_,13,9],
			[_,_,_,5,11,0,7,_,8,_,6,_,_,4,14,_]].

