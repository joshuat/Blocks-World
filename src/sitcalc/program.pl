:- include(utility).
:- include(sitcalc).
:- include(domain).
:- include(goals).

stack :-
    cputime(T1),
    	(
            stack(8)
            ;
            true
        ),
    cputime(T2),
    DT is T2-T1,
    write('Time taken: '), write(DT),nl,nl.

qstack :-
    qstack(12).


stack(Limit) :-
	stack(s0, 1, Limit).

qstack(Limit) :-
    cputime(T1),
  	(
      	quick_stack(s0, 0, Limit)
        ;
        true
    ),
    cputime(T2),
    DT is T2-T1,
    write('Time taken: '), write(DT),nl,nl.

	
not_quick_stack(N, Limit) :-
	N=<Limit,
	(
		generate_legal_situation(S, N),
		ungroup_actions(S, UngroupedS), dynamic_goal(UngroupedS),
		nl, nl, pretty_print(S), nl, nl
		;
		M is N+1,
		not_quick_stack(S, M, Limit)
	).

stack(PrevS, N, Limit) :-
    N=<Limit,
    generate_next_legal_situation(PrevS, S),
    (
        ungroup_actions(S, UngroupedS), goal3(UngroupedS),
        nl, nl, pretty_print(S), nl, nl
        ;
        M is N+1,
        stack(S, M, Limit)
    ).

quick_stack(PrevS, N, Limit) :-
    N=<Limit,
    legal_group_stack_action(PrevS, S),
    (
        ungroup_actions(S, UngroupedS),
        goal2(UngroupedS),
        nl, nl, pretty_print(S), nl, nl
        ;
        M is N+4,   % recursion adds 4 actions
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


legal_group_stack_action(PrevS, S) :-
    findall(A,agent(A),AgentList),
    % Grab action
    map(actor, GrabList, AgentList),
    not((member(G,GrabList), G\=grab(_,_))),
    map(primary_object,GrabList,ObjectList),
    not((member(O,ObjectList), on_top(_,O,PrevS))),
    legal_group_action(GrabList, PrevS),
    S1 = do(GrabList,PrevS),
    % Lift action
    map(actor, LiftList, AgentList),
    not((member(L,LiftList), L\=lift(_,_))),
    map(primary_object,LiftList,ObjectList),
    legal_group_action(LiftList, S1),
    S2 = do(LiftList,S1),
    % Put down action
    map(actor, PutList, AgentList),
    not((member(P,PutList), P\=put_down(_,_,_))),
    map(primary_object,PutList,ObjectList),
    legal_group_action(PutList, S2),
    S3 = do(PutList,S2),
    % Let go action
    map(actor, LetList, AgentList),
    not((member(M,LetList), M\=let_go(_,_))),
    map(primary_object,LetList,ObjectList),
    legal_group_action(LetList, S3),

    S = do(LetList,S3).

gen_sit(N) :-
    gen_sit2(S,s0,N,1),
	pretty_print(S),nl,nl.

gen_sit2(S,S,Limit,N) :-
    Limit < N.
gen_sit2(S,PrevS,Limit,N) :-
    N=<Limit,
    legal_group_stack_action(PrevS, S1),
%    generate_next_legal_situation(PrevS, S1),
    M is N+1,
    gen_sit2(S,S1,Limit,M).
    
