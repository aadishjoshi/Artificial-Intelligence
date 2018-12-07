%*****************************************************************************
% Sudoku 3X3
% Written By :- Aadish Joshi
%%*****************************************************************************

:- use_module(library(clpfd)). 

%*****************************************************************************
% Sudoku function
%%*****************************************************************************

sudoku3(Rows) :- length(Rows, 9), maplist(length_list(9), Rows), 
		append(Rows, Vs), 
		Vs ins 1..9, maplist(all_distinct, Rows), 
		transpose(Rows, Columns), 
		maplist(all_distinct, Columns), 
		Rows = [A,B,C,D,E,F,G,H,I], 
		blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),
		label(Vs).

length_list(L, Ls) :- length(Ls, L). 
blocks([], [], []). 
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :- all_distinct([A,B,C,D,E,F,G,H,I]), blocks(Bs1, Bs2, Bs3).

%*****************************************************************************
% File Writing
%%*****************************************************************************

write_file(Rows,_F) :-	findall(Rows, sudoku3(Rows), Sol),
			open(_F, write, ID), write(ID, Sol), close(ID).

num_sol(Rows) :- 	findall([], sudoku3(Rows), Result), length(Result,L),
			write('total '),write(L), write(' solutions output to the file: ').

%*****************************************************************************
% Input Problem generate
%%*****************************************************************************			

inp_generator(Rows,X,Y):- write('HI'),
		X>0, Y>0,
		replace(Rows,X,Y,_,Xs),
		X1 is X-1,
		Y1 is Y-1,
		inp_generator(Xs,X1,Y1),
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
			call(num_sol(Rows)), nl,
			call(sudoku3(Rows)) -> write('Solution= '),
			write(Rows),nl;
			write("No Solutions").

%*****************************************************************************
% Asserted Problem 
%%*****************************************************************************			

problem(0, P) :- 
 P = [[_,_,_,_,_,_,_,_,_],[_,_,_,_,_,3,_,8,5],[_,_,1,_,2,_,_,_,_], 	
 	      [_,_,_,5,_,7,_,_,_],[_,_,4,_,_,_,1,_,_],[_,9,_,_,_,_,_,_,_], 
	      [5,_,_,_,_,_,_,7,3],[_,_,2,_,1,_,_,_,_],[_,_,_,_,4,_,_,_,9]].

problem(1, P) :- 
        P = [[1,_,_,8,_,4,_,_,_],[_,2,_,_,_,_,4,5,6],[_,_,3,2,_,5,_,_,_],
             [_,_,_,4,_,_,8,_,5],[7,8,9,_,5,_,_,_,_],[_,_,_,_,_,6,2,_,3],
             [8,_,1,_,_,_,7,_,_],[_,_,_,1,2,3,_,8,_],[2,_,5,_,_,_,_,_,9]].

problem(2, P) :- 
        P = [[_,_,2,_,3,_,1,_,_],[_,4,_,_,_,_,_,3,_],[1,_,5,_,_,_,_,8,2],
             [_,_,_,2,_,_,6,5,_],[9,_,_,_,8,7,_,_,3],[_,_,_,_,4,_,_,_,_],
             [8,_,_,_,7,_,_,_,4],[_,9,3,1,_,_,_,6,_],[_,_,7,_,6,_,5,_,_]].

problem(3, P) :-
        P = [[1,_,_,_,_,_,_,_,_],[_,_,2,7,4,_,_,_,_],[_,_,_,5,_,_,_,_,4],
             [_,3,_,_,_,_,_,_,_],[7,5,_,_,_,_,_,_,_],[_,_,_,_,_,9,6,_,_],
             [_,4,_,_,_,6,_,_,_],[_,_,_,_,_,_,_,7,1],[_,_,_,_,_,1,_,3,_]].





