:- discontiguous trans/4, final/2, prim_action/1, natural/1, poss/3,
                 conflicts/3, start/2.

%%
%%  Provide Syntactic operators for ConGolog programs
%%
:- op(660,xfy,/).  % Nondeterministic choice
:- op(650,xfy,:).  % Sequence
:- op(640,xfy,//). % Concurrent execution
:- op(640,xfy,>>). % Prioritised concurrency
:- op(620,fx,?).   % Test

:- include(congolog).
:- include(utility).
:- include(domain).
:- include(situation).
:- include(goals).

go :- go(5).

go(Limit) :-
	write('Generating goal...'), nl,
	generate_goal(G),
	write('Looking for plans...'), nl,
	findall(S, do(go(0,Limit, G),s0,S), SList),
	write('Finding shortest plan...'), nl,
	map(sit_len, SList, LList),
	min_dual_list(SList, LList, SMin, _),
	show_action_history(SMin).

proc(go(N,Limit, Goal),
	if(goal(Goal),
		% then
		?(println('Found one.')),
		% else
		if(N<Limit,
			% then
            do_actions : pi(i,?(i is N+1) : go(i,Limit,Goal)),
			% else
			fail
		)
	)
).

proc(do_actions,
    ?(agent_list(AgentList)) : do_action(AgentList)
).

proc(do_action([]), ?(true)).
proc(do_action([Agent|List]),
    pi(a, ?(action(a,Agent)) : a) :
    do_action(List)
).


holdsInSituation(true,_).
holdsInSituation('='(A,B),_) :- A = B.
holdsInSituation('=<'(A,B),_) :- A =< B.
holdsInSituation('=>'(A,B),_) :- A >= B.
holdsInSituation('<'(A,B),_) :- A < B.
holdsInSituation('>'(A,B),_) :- A > B.
holdsInSituation(is(A,B),_) :- A is B.
holdsInSituation(goal,S) :- goal(S).
holdsInSituation(goal(G),S) :- goal(G,S).
holdsInSituation(on_top(X,Y),S) :- on_top(X,Y,S).
holdsInSituation(agent(A),_) :- agent(A).
holdsInSituation(agent_list(AgentList),_) :- findall(A,agent(A),AgentList).
holdsInSituation(action(A, Actor),S) :-
	actor(A,Actor), poss(A,S), good_action(A,S).
    % actor insists on primitive_action
holdsInSituation(good_action(A),S) :- good_action(A,S).
holdsInSituation(println(Stuff),_) :- write(Stuff), nl.
holdsInSituation(print(Stuff),_) :- write(Stuff).
holdsInSituation(printS,S) :- write(S),nl.
