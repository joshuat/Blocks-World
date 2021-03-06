%%
%%  utility.pl: A bunch of useful prolog predicates.
%%
%%  Author: Joshua Torrance (joshuat)
%%  Date: 11/11/2011
%%

%%
%%  Map Functions
%%
%%  These functions map a function to lists of arguments.
%%  eg. map(f, [a,b,c], [f(a), f(b), f(c)]).
%%
map(Function, List) :- 
	map0(Function, List).

map0(Function,[L|Ls]) :-
   Func=..[Function,L],
   call(Func),
   map(Function,Ls).
map0(_,[]).

map(Function, List, Result) :-
	map1(Function, List, Result).

map1(Function,[L|Ls],[R|Rs]) :-
   Func=..[Function,L,R],
   call(Func),
   map(Function,Ls,Rs).
map1(_,[],[]).

map(Function, List, Arg, Result) :-
	map2(Function, List, Arg, Result).
	
map2(Function,[L|Ls],[A|As], [R|Rs]) :-
   Func=..[Function,L,A,R],
   call(Func),
   map(Function,Ls,As,Rs).
map2(_,[],[],[]).


%%
%%  max(List,Max): Max is the highest value in List
%%
max([X],X).
max([X|Xs],M) :-
    max(Xs,X2),
	(
		X >= X2, M is X
		;
		X < X2, M is X2
	).


%%
%%  min(List,Min): Min is the lowest value in List
%%
min([X],X).
min([X|Xs],M) :-
    min(Xs,X2),
	(
		X =< X2, M is X
		;
		X > X2, M is X2
	).


%%
%%  list2d_to_1d(List2d, List1d): Flattens a 2d list into a 1d one.
%%
%%  Warning! Doesn't work well with [] in the 2d list.
list2d_to_1d([], []).
list2d_to_1d([[X]|Xb], [X|Ys]) :-
	list2d_to_1d(Xb, Ys).
list2d_to_1d([[X|Xa]|Xb], [X|Ys]) :-
	list2d_to_1d([Xa|Xb], Ys).


%%
%%  takeout(X,List,Remainder): Remainder is List without the element X
%%
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).


%%
%%  permute(List, Perm): Perm is a permutation of List.
%%
permute([],[]).
permute([X|Xs],Y) :- permute(Xs, Z), takeout(X,Y,Z).


%%
%%  head(List,Head): Head is the head of List.
%%
head([X|_],X).


%%
%%  distR(Elem,List,Dist): Gives the distance from the end of the list
%%  	to Elem.
%%
distR(Elem,List,D) :-
	reverse(List,RList), distL(Elem,RList,D).


%%
%%  distL(Elem,List,Dist): Gives the distance from the start of the list
%%  	to Elem.
%%
distL(Elem,[Elem|_],0).
distL(Elem,[L|Ls],D) :-
	Elem\=L, distL(Elem,Ls,D1), D is D1+1.


%%
%%  sum(List,Sum): gives the sum of the list.
%%
sum([],0).
sum([X|Xs],S) :-
	sum(Xs,S1), S is X + S1.


%%
%%  average(List,Average): gives the average of the list.
%%
average(List,Av) :-
	length(List,Len), sum(List,Sum), Av is Sum / Len.

	
%%
%%  min_dual_list(OtherList,NumList,OtherMin,Min): Finds the min element of one
%%		list and returns that and the corresponding element in another list.
%%
%%  eg. min_dual_list([a,b,c],[2,1,2],b,1).
%%
min_dual_list([G],[A],G,A).
min_dual_list([G|Gs],[A|As],Gm,Am) :-
    min_dual_list(Gs,As,G2,A2),
	(
		A =< A2, Gm = G, Am is A
		;
		A > A2, Gm = G2, Am is A2
	).


%%
%%  pretty_print(S): prints the situation legibly
%%
%%  Entry
pretty_print(s0) :- pretty_printS(s0, _).
pretty_print(do(A,S)) :- pretty_printS(do(A,S), _).
%%  Actions
pretty_printA(A) :-
	write(A).
%% Situations
pretty_printS(s0, 0) :-
	write('0: Initial Situation').
pretty_printS(do(A, S), N) :-
	pretty_printS(S, M),
	N is M+1,
	nl, write(N), write(': '),
	pretty_printA(A).