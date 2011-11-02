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
%%  unique_list(List): is true if every element in List is unique
%%
unique_list([L|Ls]) :-
	not(member(L, Ls)),
	unique_list(Ls).
unique_list([]).