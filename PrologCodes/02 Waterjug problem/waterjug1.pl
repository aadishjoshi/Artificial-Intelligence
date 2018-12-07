%% waterjug2.pl
%% 3 and 5
:- set_prolog_flag(toplevel_print_options, [quoted(true), portray(true), max_depth(100), priority(699)]). 

check_safe(G,J,s(X,Y)):- X>=0, X=<G, Y>=0, Y=<J.

move(G,J,s(X,Y),s(Z,J)) :- Z is X - (J - Y), Z >= 0.
move(G,J,s(X,Y),s(Z,0)) :- Z is X + Y, Z =< G.
move(G,J,s(X,Y),s(G,Z)) :- Z is Y - (G - X), Z >=0.
move(G,J,s(X,Y),s(0,Z)) :- Z is X + Y, Z =< J.

move(G,J,s(0,Y),s(G,Y)).
move(G,J,s(X,0),s(X,J)).
move(G,J,s(X,Y),s(X,0)) :- Y > 0.
move(G,J,s(X,Y),s(0,Y)) :- X > 0.

path(G,J,K,L,[s(X,Y)|Xs],[s(X,Y)|Xs]):- (X=K,Y=L),!.

path(G,J,K,L,[X|Xs],Rs):- 
    move(G,J,X,Y),
    check_safe(G,J,Y),
    not(member(Y,[X|Xs])),
    path(G,J,K,L,[Y,X|Xs],Rs).
	
waterjug(P,[G,J|T],[W,B|T],[K,L|T]):- path(G,J,K,L,[s(W,B)],P), reverse(P, Sol), write(Sol), nl.
% Here's an easy little predicate for printing a list.
writeOut([]).
writeOut([H|T]):-write(H),nl, writeOut(T).
