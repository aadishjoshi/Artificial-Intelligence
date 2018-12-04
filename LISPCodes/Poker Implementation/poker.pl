%*****************************************************************************
% Poker 2 player
% Written by:- Aadish Joshi
%%*****************************************************************************

:- use_module(library(random)).

shuffle:-
		%%%%%% player 1 %%%%%%
		
		random_member(U1, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y1,[hearts,clubs,spades,diamonds]),
		append([U1],[Y1],Card11),
		
		random_member(U2, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y2,[hearts,clubs,spades,diamonds]),
		append([U2],[Y2],Card12),
		
		random_member(U3, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y3,[hearts,clubs,spades,diamonds]),
		append([U3],[Y3],Card13),
		
		random_member(U4, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y4,[hearts,clubs,spades,diamonds]),
		append([U4],[Y4],Card14),
		
		random_member(U5, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y5,[hearts,clubs,spades,diamonds]),
		append([U5],[Y5],Card15),
		
		%%%%%% player 2%%%%%%
		
		random_member(U6, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y6,[hearts,clubs,spades,diamonds]),
		append([U6],[Y6],Card21),

		random_member(U7, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y7,[hearts,clubs,spades,diamonds]),
		append([U7],[Y7],Card22),
		
		random_member(U8, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y8,[hearts,clubs,spades,diamonds]),
		append([U8],[Y8],Card23),
		
		random_member(U9, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y9,[hearts,clubs,spades,diamonds]),
		append([U9],[Y9],Card24),
		
		random_member(U10, [2,3,4,5,6,7,8,9,10,jack,queen,king,ace]),
		random_member(Y10,[hearts,clubs,spades,diamonds]),
		append([U10],[Y10],Card25),
		
		H1 = [Card11, Card12,Card13,Card14,Card15], 
		H2 = [Card21,Card22,Card23,Card24,Card25],
		%H1 = [[2,diamonds], [4,hearts],[7, clubs],[7,diamonds],[9,hearts]], 
		%H2 = [[3,diamonds],[4,hearts],[5,clubs],[2,hearts],[8,clubs]], 
		winner(H1,H2, Winner),!,
		print(H1),format('~n'),nl,print(H2),
		print('win='), print(Winner).

play([],0).
play([[Hand1,Hand2]|Rst], Num_Wins) :-
  winner(Hand1, Hand2, Winner),
  (Winner = Hand1, play(Rst,Remaining), Num_Wins is 1 + Remaining ;
   play(Rst, Num_Wins)).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cumulative probabilities

cumulative_prob(royal_flush, 0.00056).   	% 1/ 11*10*4*4   
cumulative_prob(straigh_flush, 0.00056). 	% 1/ 4*4*11*10
cumulative_prob(flush, 0.0625). 			% 1/4*4
cumulative_prob(straight, 0.0068). 			% 4/49 * 4/48
cumulative_prob(four_of_a_kind, 0.0041).	% 1/14 * 1/4 * 13/14 * 1/4
cumulative_prob(three_of_a_kind, 0.0538).	% 13/14 * 1/4 * 13/14 * 1/4
cumulative_prob(two_pair, 0.056).			% 3/49 * 44/49.
cumulative_prob(high_card, 0.919).			% 47/49 * 46/48.
cumulative_prob(pair, 0.056).			% 47/49 * 46/48.
   
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  find Rank.

find_rank([C11,C12,C13|T1], R1):-
	guess_hand([C11,C12,C13], Z1),
	format('On table: ~n'),
	print(C11),
	print(C12),
	print(C13),nl,
	format('Might be: ~n'),
	print(Z1),nl,
	cumulative_prob(Z1,P),
	format('Cumulative prob of occurance is :'),
	print(P),
	guess_rank(Z1,R1).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Compare Rank.
	
compare_rank(N,R1):-
	(R1 = N ->
		format('Equiprobable ranks. Can take a chance with 3 coins ~n'));

	% hisrank < myRank	
	(R1 < N -> 
		
		format('Probably W2 will win. Do not play ~n'),
		(R1 > 5 -> 
			print('Can take a chance with more 3 coins'); 
			print('Quit or can bet less than 3 coins')));
	
	% hisrank > myRank
	(R1 > N ->
		format('W1 will win. Take a chance ~n'),
		(R1 > 5 -> 
			print('can bet more than 6 coins'); 
			print('but less than 6 coins'))).
		

%%%%%%%%%%%%%%%%%%%
% Guess ranks
guess_rank(royal_flush,1).
guess_rank(straigh_flush,2).
guess_rank(flush,3).
guess_rank(straight,4).
guess_rank(three_of_a_kind,5).
guess_rank(pair,6).
guess_rank(high_card,7).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Guess Hand determination
%% royal_flush= 1;straigh_flush = 2; flush = 3;
%% straight = 4; three_of_a_kind = 5; pair = 6;

guess_hand([[10,X],[jack,X],[queen,X]], royal_flush).
guess_hand([[10,X],[jack,X],[ace,X]], royal_flush).
guess_hand([[10,X],[jack,X],[king,X]], royal_flush).
guess_hand([[10,X],[queen,X],[king,X]], royal_flush).
guess_hand([[10,X],[queen,X],[ace,X]], royal_flush).
guess_hand([[10,X],[king,X],[ace,X]], royal_flush).
guess_hand([[10,X],[queen,X],[king,X]], royal_flush).
guess_hand([[jack,X],[queen,X],[king,X]], royal_flush).
guess_hand([[jack,X],[queen,X],[ace,X]], royal_flush).
guess_hand([[jack,X],[king,X],[ace,X]], royal_flush).
guess_hand([[queen,X],[king,X],[ace,X]], royal_flush).

guess_hand([[A,X],[B,X],[C,X]], straight_flush) :-
  successor(C,B), successor(B,A).

guess_hand([[_,X],[_,X],[_,X]], flush).

guess_hand([[A,_],[B,_],[C,_]], straight) :-
  successor(C,B), successor(B,A).
 
guess_hand([[A,_],[B,_],[C,_]], three_of_a_kind) :-
  (A = B, B = C).

guess_hand([[A,_],[B,_],[C,_]], pair) :-
  A = B; B = C.

guess_hand(_,high_card).  
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Playing the game.
winner(H1, H2, Winner) :-
  sort_hand(H1, Sorted_Hand1),
  sort_hand(H2, Sorted_Hand2),
  
 
  find_rank(Sorted_Hand1,Myrank),
  format('~n'),
  format('My Rank: '),
  print(Myrank),nl,
  format('~n'),
  
  find_rank(Sorted_Hand2,Hisrank),
  format('~n'),
  format('Hisrank Rank: '),
  print(Hisrank),nl,
  format('~n'),
  
  compare_rank(Myrank,Hisrank),
  format('~n'),
  format('Actual Reveal: '),
  format('~n'),

  determine_hand(Sorted_Hand1,  X1),
  determine_hand(Sorted_Hand1,  X1),
  determine_hand(Sorted_Hand2,  X2),
  beats(X1, X2, Verdict),
  (Verdict = X1, Winner = H1;
   Verdict = X2, Winner = H2;
   Verdict = tie, tiebreak(X1, Sorted_Hand1, Sorted_Hand2, SortedWinner),
   (SortedWinner = left, Winner = H1 ;
    SortedWinner = right, Winner = H2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Tiebreaks
tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
tiebreak(four_of_a_kind, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).
tiebreak(full_house, H1, H2, Winner)      :- higher_middle_card(H1, H2, Winner).
tiebreak(flush, H1, H2, Winner)           :- tiebreak(high_card, H1, H2, Winner).
tiebreak(straight, H1, H2, Winner)        :- higher_last_card(H1, H2, Winner).
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).

tiebreak(two_pair, H1, H2, Winner) :-
  isolate_pairs(H1, [HighCard1,_], [LowCard1,_], Last1),
  isolate_pairs(H2, [HighCard2,_], [LowCard2,_], Last2),
  (beats_with_hand(H1, HighCard1, H2, HighCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, LowCard1, H2, LowCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, Last1, H2, Last2, Winner)).
     
tiebreak(pair, H1, H2, Winner) :-
  isolate_pair(H1, [PairCard1,_], Rst1),
  isolate_pair(H2, [PairCard2,_], Rst2),
  (beats_with_hand(H1, PairCard1, H2, PairCard2, Winner), Winner \= tie ;
   tiebreak(high_card, Rst1, Rst2, Winner)).

tiebreak(high_card, H1, H2, X) :- 
  reverse(H1, RevH1),
  reverse(H2, RevH2),
  highest_card_chain(RevH1, RevH2, X).


beats_with_hand(H1, C1, H2, C2, X) :-
  beats(C1, C2, C1), X = left ;
  beats(C1, C2, C2), X = right ;
  X = tie.

% Really ugly.  How to better do this?
isolate_pairs(Hand, High_Pair, Low_Pair, Last) :-
  [[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
  (V5 = V4, High_Pair = [[V4,S4],[V5,S5]],
    (V3 = V2, Low_Pair = [[V3,S3],[V2,S2]], Last = [V1,S1] ;
     V1 = V2, Low_Pair = [[V1,S1],[V2,S2]], Last = [V3,S3])) ;
  (Low_Pair = [[V1,S1],[V2,S2]], 
   High_Pair = [[V3,S3],[V4,S4]],
   Last = [V5,S5]).

isolate_pair(Hand, Pair, Rst) :-
  [[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
  (V1 = V2, Pair = [[V1,S1],[V2,S2]], Rst = [[V3,S3],[V4,S4],[V5,S5]] ;
   V2 = V3, Pair = [[V3,S3],[V2,S2]], Rst = [[V1,S1],[V4,S4],[V5,S5]] ;
   V4 = V3, Pair = [[V3,S3],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V5,S5]] ;
   V4 = V5, Pair = [[V5,S5],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V3,S3]]).
  

highest_card_chain([H1|T1], [H2|T2], X) :-
  beats(H1,H2,Verdict),
  (Verdict = H1, X = left ;
   Verdict = H2, X = right ;
   Verdict = tie, highest_card_chain(T1,T2,X)).

higher_last_card(H1,H2,Winner) :-
  H1 = [_,_,_,_,[V1,_]],
  H2 = [_,_,_,_,[V2,_]],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left ;
   Higher = V2, Winner = right).

higher_middle_card(H1, H2, Winner) :-
  H1 = [_,_,[V1,_],_,_],
  H2 = [_,_,[V2,_],_,_],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left;
   Higher = V2, Winner = right).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand determination
determine_hand([[10,X],[jack,X],[queen,X],[king,X],[ace,X]], royal_flush).

determine_hand([[A,X],[B,X],[C,X],[D,X],[E,X]], straight_flush) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[C,_],[A,_],[A,_],[A,_],[B,_]], four_of_a_kind) :-
  C = A ; B = A.

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], full_house) :-
  A = B, D = E, (C = D ; C = B).

determine_hand([[_,X],[_,X],[_,X],[_,X],[_,X]], flush).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], straight) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], three_of_a_kind) :-
  (A = B, B = C); (B = C, C = D); (C = D, D = E).

determine_hand([[A,_],[A,_],[B,_],[B,_],[_,_]], two_pair).
determine_hand([[_,_],[A,_],[A,_],[B,_],[B,_]], two_pair).
determine_hand([[A,_],[A,_],[_,_],[B,_],[B,_]], two_pair).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], pair) :-
  A = B; B = C; C = D; D = E.

determine_hand(_,high_card).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand sorting (for easier pattern matching).
sort_hand([], []).
sort_hand([H|T], Sorted) :-
  filter_by_high_card(H,T,Lower,Higher),
  sort_hand(Lower,SortedLower),
  sort_hand(Higher,SortedHigher),
  append(SortedLower, [H|SortedHigher], Sorted).


filter_by_high_card(_, [], [], []).  
filter_by_high_card(Pivot, [H|T], [H|Lower], Higher) :-
  beats(Pivot,H,Z),
  (Z = Pivot ; Z = tie),
  filter_by_high_card(Pivot, T, Lower, Higher).
filter_by_high_card(Pivot, [H|T], Lower, [H|Higher]) :-
  beats(Pivot,H,H),
  filter_by_high_card(Pivot, T, Lower, Higher).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Card and Hand Precedence
beats([V,_],[V,_],tie).
beats([V1,S],[V2,_],[V1,S]) :- value_greater_than(V1,V2).
beats([V1,_],[V2,S],[V2,S]) :- value_greater_than(V2,V1).

beats(X,X,tie).
beats(X,Y,X) :- value_greater_than(X,Y).
beats(X,Y,Y) :- value_greater_than(Y,X).

successor(royal_flush, straight_flush).   successor(straigh_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).    successor(full_house, flush).
successor(flush, straight).               successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).     successor(two_pair, pair).
successor(pair, high_card).

successor(ace,king).     successor(king,queen).   successor(queen,jack).
successor(jack,10).      successor(10,9).         successor(9,8).
successor(8,7).          successor(7,6).          successor(6,5).
successor(5,4).          successor(4,3).          successor(3,2).

value_greater_than(X,Y) :-
  successor(X,P),
  (Y = P;
  value_greater_than(P,Y)).

