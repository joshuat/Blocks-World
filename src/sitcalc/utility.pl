%%
%%  Map Functions
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

%%
%%  max(List,Max): Max is the highest value in List
%%
max([X],X).
max([X|Xs],X) :-
    max(Xs,X2), X >= X2.
max([X|Xs],X2) :-
    max(Xs,X2), X2 > X.

%%
%%  unique_list(List): is true if every element in List is unique
%%
unique_list([L|Ls]) :-
	not(member(L, Ls)),
	unique_list(Ls).
unique_list([]).

%%
%%  head(List,Head): Head is the head of List.
%%
head([X|_],X).

%%
%%  pretty_print(S): prints the situation legibly
%%
%%  Entry
pretty_print(s0) :- pretty_print(s0, _).
pretty_print(do(A,S)) :- pretty_print(do(A,S), _).
%%  Actions
pretty_print([A]) :-
	write('\t'), write(A).
pretty_print([A|As]) :-
	write('\t'), write(A), write(','), nl,
	pretty_print(As).
%% Situations
pretty_print(s0, 0) :-
	write('0: Initial Situation').
pretty_print(do(A, S), N) :-
	pretty_print(S, M),
	N is M+1,
	nl, write(N), write(': '),
	pretty_print(A).
	

