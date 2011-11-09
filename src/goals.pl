goal1(S) :-
    on_top(d, c, S),
    on_top(c, b, S),     on_top(g, f, S),
    on_top(b, a, S),     on_top(f, e, S),
    on_top(a, floor, S), on_top(e, floor, S).

goal2(S) :-
    on_top(a, b, S),
    on_top(b, c, S),
    on_top(c, d, S),     on_top(e, f, S),
    on_top(d, floor, S), on_top(f, floor, S), on_top(g, floor, S).

	
goal3(S) :-
	on_top(a, b, S),
	on_top(b, floor, S),	on_top(c, floor,S).


goal(G,S) :-
	check_stacks(G,S).

check_stacks([], _).
check_stacks([G|Gs], S) :-
	check_stack(G, floor, S), check_stacks(Gs, S).

check_stack([], G, S) :-
	\+ on_top(_,G,S).
check_stack([G|Gs], Place, S) :-
	on_top(G, Place, S), check_stack(Gs, G, S).


%%
%%  generate_goal(Goal): generates a goal state
%%
%%  This predicate generates a goal state of the form
%%  [ [a, b, c], [d, e] ]
%%  which would represent
%%  c
%%  b e
%%  a d
%%
%%  Several fairly arbiraty rules are used to generate the goal.
generate_goal(Goal) :-
	findall(G,
		(% All blocks must be in the goal.
		all_blocks_in_goal(G),
		% minimise the number of stacks.
		ideal_num_stacks(NStacks), length(G,NStacks),
		% Pu the heaviest block on the bottom.
		heaviest_on_bottom(G)),
		GoalList),
	length(GoalList,L),
	write('Number of possible goals: '), write(L), nl,
	min_access_time(GoalList, Goal),
	write('Best goal for quick unstacking is:'), nl,
	write(Goal), nl.
	
min_access_time(GoalList,Goal) :-
	map(average_access, GoalList, AverageList),
	min_dual_list(GoalList, AverageList, Goal, _).






%%
%% Goal Utility Functions
%%

%%
%%  max_height(Height): gives the maximum height of a stack
%%
%%  ASSUMPTION: the heaviest block can be lifted to the highest height.
%%  It'd be nice to get rid of this assumption without a horrendously
%%  complicated predicate.
%%
max_height(HMax) :-
    findall(R,robot(R),RList), map(height,RList,HList), max(HList,HMax).

%%
%%  num_blocks(N): gives the number of blocks in the domain
%%
num_blocks(N) :-
    findall(B,block(B),BList), length(BList,N).

%%
%% ideal_num_stacks(N): gives the ideal (lowest) number of stacks
%%
ideal_num_stacks(N) :-
    max_height(H), num_blocks(B),
    N is ceiling(B / H).
	
%%
%%  heaviest_on_bottom(Stacks): true if the blocks with the biggest
%%      weight are below those with less.
%%
heaviest_on_bottom([S]) :-	% Deals with a list of stacks
    heaviest_on_bottom2(S).
heaviest_on_bottom([S|Ss]) :-
    heaviest_on_bottom2(S), heaviest_on_bottom(Ss).

heaviest_on_bottom2([_]).	% Deals with stacks
heaviest_on_bottom2([B1|Bs]) :-
    head(Bs,B2),
    weight(B1,W1), weight(B2,W2), W1>=W2,
    heaviest_on_bottom2(Bs).
	
%%
%%  all_blocks_in_goal(Goal): true if all blocks in the domain are in
%%  	the goal list.
%%
all_blocks_in_goal(Goal) :-
	findall(B,block(B),BlockList),
	permute(BlockList,Goal1d),
	list2d_to_1d(Goal,Goal1d).
	
%%
%%  average_access(StackList, AvAccessTime): this predicate calculates the
%%		average access time for each type of block.
%%
average_access(StackList, Time) :-
	findall(T,type(T),TypeList),
	average_access2(StackList, TypeList, TimeList),
	average(TimeList, Time).

average_access2(_, [], []).
average_access2(StackList, [T|Ts], [Time|Times]) :-
	average_access3(StackList, T, Time), average_access2(StackList, Ts, Times).
	
average_access3(StackList, T, Time) :-
	findall(D,
		(member(Stack,StackList), member(Block,Stack), type(Block,T), distR(Block,Stack,D)),
		Depths),
	length(Depths,L),
	(
		L>0, min(Depths, Time)
		;
		L=<0, Time is 0
	).
	

%%
%%  min_dual_list(OtherList,NumList,OtherMin,Min): Finds the min element of one
%%		list and returns that and the corresponding element in another list.
%%
min_dual_list([G],[A],G,A).
min_dual_list([G|Gs],[A|As],Gm,Am) :-
    min_dual_list(Gs,As,G2,A2),
	(
		A =< A2, Gm = G, Am is A
		;
		A > A2, Gm = G2, Am is A2
	).
