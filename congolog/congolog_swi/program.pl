stack :-
	stack(3).

stack(Limit) :-
	quick_stack(s0, 1, Limit)
	;
	write('No solution found in '),
	write(Limit),
	write(' actions.'), nl, nl.
	
not_so_quick_stack(N, Limit) :-
	N=<Limit,
	(
		generate_legal_situation(S, N),
		ungroup_actions(S, UngroupedS), dynamic_goal(UngroupedS),
		nl, nl, pretty_print(S), nl, nl
		;
		M is N+1,
		stack2(S, M, Limit)
	).

quick_stack(PrevS, N, Limit) :-
    N=<Limit,
    generate_next_legal_situation(PrevS, S),
    (
        ungroup_actions(S, UngroupedS), dynamic_goal(UngroupedS),
        nl, nl, pretty_print(S), nl, nl
        ;
        M is N+1,
        quick_stack(S, M, Limit)
    ).


generate_legal_situation(s0, 0).
generate_legal_situation(do(A, S), N) :-
	N>0,
	M is N-1,
	generate_legal_situation(S, M),
	group_action(A),
	legal_group_action(A, S).

generate_next_legal_situation(PrevS, do(A, PrevS)) :-
    group_action(A),
    legal_group_action(A, PrevS).

gen_sit(N) :-
	generate_legal_situation(S, N),
	pretty_print(S),nl,nl.
