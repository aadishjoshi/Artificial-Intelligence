%*****************************************************************************
% CLP Program 1
% Written by:- Aadish Joshi
%%*****************************************************************************

:- use_module(library(clpfd)).

%*****************************************************************************
% Multiply by 10 functionality
%%*****************************************************************************
add([],A,0).
add([A|B],C,D):-
	B1 #= A*C,
	C1 is C*10,
	add(B,C1,B2),
	D #= B1 + B2.

%*****************************************************************************
% Puzzle code program
%%*****************************************************************************
	
puzzle(Aa + Bb = Cc):-
	term_variables([Aa|Bb],Var2),
	term_variables([Var2|Cc],Var),
	Var ins 0..9, %range
	all_different(Var),
	reverse(Aa,Ax),	reverse(Bb,Bx), reverse(Cc,Cx),
	add(Ax,1,Ay), add(Bx,1,By), add(Cx,1,Cy),
	Ay + By #= Cy, 
	test(Aa), test(Bb), test(Cc),
	label(Aa), label(Bb), label(Cc).
	
test([A|B]):-
	A #\= 0.

