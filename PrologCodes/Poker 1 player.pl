%*****************************************************************************
% Poker 1 player
% Written by:- Aadish Joshi
%%*****************************************************************************

:- use_module(library(random)).

%*****************************************************************************
% Suits and Face
%%*****************************************************************************
suit(hearts).
suit(clubs).
suit(spades).
suit(diamonds).

face(0).
face(1).

%*****************************************************************************
% card definition
%%*****************************************************************************
card(I, J) :-
		integer(I),
		suit(J).
%*****************************************************************************
% Display function
%%*****************************************************************************
display([H|T]):-
		write(H),
		display(T).

		
%*****************************************************************************
% Flush and StraighFlush
%%*****************************************************************************
		
flush(X) :-
		nth1(1, X, A),
		nth1(2, X, B),
		nth1(3, X, C),

		A = card(_F, Z),
		B = card(_G, Z),
		C = card(_H, Z).


straight(X) :-
		nth1(1, X, A),
		nth1(2, X, B),
		nth1(3, X, C),

		A = card(F, _K),
		B = card(G, _L),
		C = card(H, _M),

		Values = [F, G, H],
		sort(Values, Sorted),
		nth1(1, Sorted, A1),
		nth1(2, Sorted, B1),
		nth1(3, Sorted, C1),
		C1 - B1 =:= 1,
		B1 - A1 =:= 1.
		

straightflush(X) :-
		straight(X),
		flush(X).

		
%*****************************************************************************
% Compared Cards
%%*****************************************************************************

comparecards(N, A, B, C) :-

		N =:= 3,
		A =:= B,
		B =:= C,
		write("Predicted Probability Three of a Kind: 2.1128% "),
		nl,
		write("Could be Four of a Kind: 0.0240% "),
		nl,
		write("Could be Royal Flush: 0.000154% "),
		nl,
		format(' ~n');


		N =:= 3,
		write("Probability Two Pair: 4.7539%%"),
		nl,
		format(' ~n');

		N =:= 2,
		A =:= B,
		B =:= C,
		write("	Probability Full House: 0.1441%"),
		format(' ~n');

		N =:= 2,
		A =:= B,
		write("Probability Two Pair: 4.7539%%"),
		nl,
		format('Probability Full House ~n');

		N =:= 2,
		write("Probability Two Pair: 4.7539%%"),
		nl,
		format('Probability Four of a Kind:  ~n');

		write("Probability Two Pair: 42.2569%"),
		nl,
		format('Nothing ~n').

%*****************************************************************************
% Sorted cards
%%*****************************************************************************
		
prep(X) :-
		nth1(1, X, A),
		nth1(2, X, B),
		nth1(3, X, C),

		A = card(F, _R),
		B = card(G, _S),
		C = card(H, _T),

		Y = [F, G, H],

		sort(Y, Sorted),
		msort(Y, SortedM),
		length(Sorted, Q),
		nth1(1, SortedM, K),
		nth1(2, SortedM, L),
		nth1(3, SortedM, M),

		comparecards(Q, K, L, M).	
		
%*****************************************************************************
% Classify pattern
%%*****************************************************************************
	
classify(X) :-
		straightflush(X),
		format('Straight Flush ~n');
		flush(X),
		format('Flush ~n');
		straight(X),
		format('Straight ~n');

		prep(X).

%*****************************************************************************
% Shuffle deck.
%%*****************************************************************************
		
shuffle:-
		random(1,14,U),
		random_member(Y,[hearts,clubs,spades,diamonds]),
		append([U],[Y],Card11),
		append(Card11,[1],Card1),
		
		random(1,14,U1),
		random_member(Y1,[hearts,clubs,spades,diamonds]),
		append([U1],[Y1],Card12),
		append(Card12,[1],Card2),
		
		random(1,14,U2),
		random_member(Y2,[hearts,clubs,spades,diamonds]),
		append([U2],[Y2],Card13),
		append(Card13,[1],Card3),
		
		random(1,14,U3),
		random_member(Y3,[hearts,clubs,spades,diamonds]),
		append([U3],[Y3],Card14),
		append(Card14,[0],Card4),
		
		random(1,14,U4),
		random_member(Y4,[hearts,clubs,spades,diamonds]),
		append([U4],[Y4],Card15),
		append(Card15,[0],Card5),
		
		classify([card(U,Y),card(U1,Y1),card(U2,Y2)]),
		
		
		write("Card1:"),
		write(Card1),
		nl,
		write("Card2:"),
		write(Card2),
		nl,
		write("Card3:"),
		write(Card3),
		nl,
		nl,
		write("Actual Hidden Cards:"),
		write("Card4:"),
		write(Card4),
		nl,
		write("Card5:"),
		write(Card5).