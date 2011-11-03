goal(S) :-
    on_top(block1, block2, S).

	
stack(S) :-
	stack(S, 4).

stack(S, Limit) :-
	stack2(S, 0, Limit).
	
stack2(S, N, Limit) :-
	N<Limit,
	(
		generate_legal_situation(S, N),
		ungroup_actions(S, UngroupedS), goal(UngroupedS)
		;
		M is N+1,
		stack2(S, M, Limit)
	).
	

generate_legal_situation(S, N) :-
		generate_situation2(S, N),
		legal(S).

generate_situation2(s0, 0).
generate_situation2(do(A, S), N) :-
	N>0,
	M is N-1,
	group_action(A),
	generate_situation2(S, M).
	