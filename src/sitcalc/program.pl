%%
%%  program.pl: High level procedures for the extended blocks world implementation.
%%
%%  Author: Joshua Torrance (joshuat)
%%  Date: 11/11/2011
%%
%%  The dependents for this program are:
%%  -utility.pl:	A bunch of useful predicates.
%%  -sitcalc.pl:	A implementation of the situation calulus that allows agents
%%					to take simultaneous cooperative actions.
%%  -domain.pl		An extended blocks world definition.
%%  -goals.pl		Goals and utility predicate for goal.
%%

%%
%%  Include the relevant definitions.
%%
:- include(utility).
:- include(sitcalc).
:- include(domain).
:- include(goals).

%%
%%  qstack: A relatively efficient stacking procedure.
%%
%%  qstack acquires its efficiency by forcing each agent to
%%  choose sets of grab, lift, put_down and let_ actions each
%%  iteration removing a lot of checks but potentially reducing
%%  final plan efficiency since cunning interleaving of the four
%%  actions is not possible.
%%
qstack :-
    cputime(T1),
    qstack(4),
    cputime(T2),
    DT is T2-T1,
    write('Time taken: '), write(DT),nl,nl.

qstack(Limit) :-
    quick_stack(s0, 1, Limit).

quick_stack(PrevS, N, Limit) :-
    N=<Limit,
    legal_group_stack_action(PrevS, S),
    (
        ungroup_actions(S, UngroupedS),
        goal(UngroupedS),
        nl, nl, pretty_print(S), nl, nl
        ;
        M is N+4,   % recursion adds 4 actions
        quick_stack(S, M, Limit)
    ).

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


%%
%%  stack: a less efficient but more precise stacking procedure.
%%
stack :-
    cputime(T1),
    stack(4),
    cputime(T2),
    DT is T2-T1,
    write('Time taken: '), write(DT),nl,nl.

stack(Limit) :-
	stack(s0, 1, Limit).

stack(PrevS, N, Limit) :-
    N=<Limit,
    generate_next_legal_situation(PrevS, S),
    (
        ungroup_actions(S, UngroupedS), goal(UngroupedS),
        nl, nl, pretty_print(S), nl, nl
        ;
        M is N+1,
        stack(S, M, Limit)
    ).
	
generate_next_legal_situation(PrevS, do(A, PrevS)) :-
    group_action(A),
    legal_group_action(A, PrevS).
	


%%
%%  generate_legal_situation(S,N): S is a legal situation
%%									of length N.
%%
generate_legal_situation(s0, 0).
generate_legal_situation(do(A, S), N) :-
	N>0,
	M is N-1,
	generate_legal_situation(S, M),
	group_action(A),
	legal_group_action(A, S).

