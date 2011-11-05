goal(S) :-
    on_top(block1, block2, S).

dynamic_goal(S) :-
	block_list(BList), length(BList,BListLen),
	WORK IN PROGRESS!!
	
	
stack :-
	stack(_, 3).

stack(S, Limit) :-
	stack2(S, 1, Limit)
	;
	write('No solution found in '),
	write(Limit),
	write(' actions.'), nl, nl.
	
stack2(S, N, Limit) :-
	N=<Limit,
	(
		generate_legal_situation(S, N),
		ungroup_actions(S, UngroupedS), goal(UngroupedS),
		nl, nl, pretty_print(S), nl, nl
		;
		M is N+1,
		stack2(S, M, Limit)
	).


generate_legal_situation(s0, 0).
generate_legal_situation(do(A, S), N) :-
	N>0,
	M is N-1,
	generate_legal_situation(S, M),
	group_action(A),
	legal_group_action(A, S).
	

gen_sit(N) :-
	generate_legal_situation(S, N),
	pretty_print(S),nl,nl.