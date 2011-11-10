%%
%%  program.pl: Top-level prolog file for ConGolog implementation.
%%
%%  Author: Joshua Torrance (joshuat)
%%  Date: 11/11/2011
%%
%%  In order to execute the program at the prolog interpreter prompt:
%%		?- consult(program).	% Don't worry about the style warnings.
%%		?- go.
%%
%%  This file intends to be domain independent of a particular situation.
%%
%%  In order to define a particular domain see:
%%  -domain.pl		Definitions of the domain such as possibility axioms,
%%					successor state axioms and good action choice axioms.
%%  -situation.pl	Definitions of objects and agents in the domain as
%%					well as the initial situation.
%%  -goal.pl		Definitions of the goals of system.
%%
%%  The other dependents are:
%%  -congolog.pl	Ryan Kelly's (slightly edited) ConGolog implementation.
%%					The changes are clearly marked with EDIT.
%%  -utility.pl		A whole bunch of useful functions used by the program.
%%
%%  This file is roughly based of Ryan Kelly's main.pl inplementation of
%%  the cooking domain.
%%
%%  TODO: Implement true concurrency.
%%  TODO: Speed up execution with more intelligent action selection.
%%

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

%%
%%  Include the relevant definitions.
%%
:- include(congolog).
:- include(utility).
:- include(domain).
:- include(situation).
:- include(goals).

%%
%%  go: Main entry point for the progrom.
%%
go :- go(5).

%%
%%  go(Limit): High level program. Limit is the maximum number of actions.
%%
go(Limit) :-
	write('Generating goal...'), nl,
	generate_goal(G),
	write('Looking for plans...'), nl,
	findall(S, do(go(0,Limit, G),s0,S), SList),
	write('Finding shortest plan...'), nl,
	map(sit_len, SList, LList),
	min_dual_list(SList, LList, SMin, _),
	show_action_history(SMin).


%%
%%  go(N,Limit,Goal): Primary ConGolog procedure.
%%
%%  N is the current iteration, Limit is the maximum number of iterations,
%%  Goal is the situation we're trying to achieve (tested with goal(Goal)).
%%
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


%%
%%  do_actions: gets each agent in the system to perform an action.
%%
%%  True concurrency has not yet been implemented. Currently the agents
%%  just take turns performing actions.
%%
proc(do_actions,
    ?(agent_list(AgentList)) : do_action(AgentList)
).

proc(do_action([]), ?(true)).
proc(do_action([Agent|List]),
    pi(a, ?(action(a,Agent)) : a) :
    do_action(List)
).


%%
%%  agent_list(A): retrieves a list of all agents in the system.
%%
holdsInSituation(agent_list(AgentList),_) :- findall(A,agent(A),AgentList).


%%
%%  action(Action,Actor): determines an action for Actor.
%%
%%  poss and good_action should be defined in the domain.
%%
holdsInSituation(action(A, Actor),S) :-
	actor(A,Actor), poss(A,S), good_action(A,S).
    % actor insists on primitive_action

%%
%%  actor(Actn,Agt):  performing agent for Actions
%%
%%  This predicate binds Agt to the agent performing primitive action Actn.
%%
actor(Actn,Agt) :-
    primitive_action(Actn), arg(1,Actn,Agt).


%%
%% Some useful print 'test' predicates.
%%
holdsInSituation(println(Stuff),_) :- write(Stuff), nl.
holdsInSituation(print(Stuff),_) :- write(Stuff).
holdsInSituation(printS,S) :- write(S),nl.
