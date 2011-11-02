%%
%%  sitcalc.pl:  Prolog Implementation of the Situation Calculus
%%
%%  Author:  Ryan Kelly (rfk)
%%
%%  Date Created:  12/03/07
%%
%%  To implement a domain axiomatisation using this framework, the following
%%  tasks must be completed:
%%
%%       * specify the primitive actions of the world using primitive_action/1.
%%
%%       * specify the primitive fluents in the world using prim_fluent/1.
%%
%%       * specify the agents in the system using agent/1.
%%
%%       * specify the possibility axioms for primitive actions using poss/3.
%%
%%       * specify the successor state axioms in terms of predicates for
%%         each fluent.
%%
%%       * specify the initial conditions using fluent predicates with the
%%         situation term set to s0.
%%


%%
%%  actor(Actn,Agt):  performing agent for Actions
%%
%%  This predicate binds Agt to the agent performing primitive action Actn.
%%
actor(Actn,Agt) :-
    primitive_action(Actn), arg(1,Actn,Agt).

%%
%%  precedes(S1,S2):  ordering over situations
%%
%%  This predicate is true when S2 is reachable from S1 by some finite
%%  sequence of actions.  Note that no situation precedes s0, by definition.
%%
precedes(_,s0) :- fail.
precedes(S1,do(A,S2)) :-
    poss(A,S2), precedes_eq(S1,S2).

%%
%%  precedes_eq(S1,S2):  precedes-or-equals
%%
%%  This predicate is to precedes/2 as <= is to <, it allows for the
%%  two arguments to be equal.
%%
precedes_eq(S1,S2) :-
    S1 = S2 ; precedes(S1,S2).

%%
%%  poss(A,S):   possibility of executing an action
%%
%%  The predicate poss/3 must be true whenever it is possible to perform
%%  action A in situation S.
%%
%%  The domain axiomatiser is required to provide implementations of
%%  poss/3 for all primitive actions.
%%


%%
%%  group_action(ListPrimAct): a list of primitive actions
%%
%%  A group action is a list of primitive actions with one action for
%%  each agent in the system.
%%
group_action(List) :-
	agent_list(AgentList),
	map(actor, List, AgentList),
	map(primitive_action, List),
	unique_list(AgentList).				% GET UNWANTED PERMUTATIONS HERE :(
	
	
%%
%%  legal(S):    legality of a situation
%%
%%  The predicate legal/3 must be true if the situation S is composed
%%  of legal actions. Actions are legal if they are possible in the
%%  given situation.
legal(s0).
legal(do(A, S)) :-
	primitive_action(A), poss(A, S), legal(S)
	;
	group_action(A), legal_group_action(A, S), legal(S).

legal_group_action([], _).
legal_group_action([A|As], S) :-
	no_similar_actions(A, As),
	ungroup_actions(S, UngroupedS), poss(A, UngroupedS),
	legal_group_action(As, S).

%%
%%  no_similar_actions(Action, List): true is none of the actions are 'similar'
%%
%%  This predicate is true if none of the actions are similar. Eg. no robot is
%%  trying to pick up a box another robot is picking up.
%%  Similar is difined in the domain.
no_similar_actions(_, []).
no_similar_actions(Act, [A|As]) :-
	no_similar_actions2(Act, [A|As]),
	no_similar_actions(A, As).
no_similar_actions2(_, []).
no_similar_actions2(Act, [A|As]) :-
	not(similar_action(Act, A)),
	no_similar_actions2(Act, As).

%%
%%  ungroup_actions(GSit, USit): flattens a situation of grouped actions
%%  into a situation without grouped actions. This is intended for use
%%  with poss.
%%
ungroup_actions(s0, s0).
ungroup_actions(do([], S), US) :-
	ungroup_actions(S, US).
ungroup_actions(do([A|As], S), do(A, US)) :-
	ungroup_actions(do(As, S), US).


%%
%%  history_length(N,S):  length of the action histoy in a situation
%%
%%  This simple fluent encodes in N the number of actions that have
%%  taken place in the history of situation S.  It is used to make this
%%  information easily available to agents.
%%
history_length(0,s0).
history_length(N,do(_,S)) :-
	N>0,
    history_length(N1,S),
    N is N1 + 1.
