%%
%%  congolog.pl:  Basic implementation of ConGolog semantics
%%
%%  Author:  Ryan Kelly (rfk)
%%
%%  Date Created:  12/03/07
%%
%%    This is an re-implementation of the transition semantics for ConGolog
%%    given by De Giacomo et al ("ConGolog, a concurrent programming language
%%    based on the situation calculus").
%%
%%    TODO:  document how programs are formed
%%
%%    The predicate do(D,S,Sp) will take a program D and initial situation
%%    S and find a situation Sp in which the program will legally terminate.
%%    The actions performed by the program are available in the action
%%    history of Sp.  This predicate will backtrack over possible transitions
%%    until a legal execution is found, and will determine more legal
%%    executions when it is backtracked.
%%
%%  Some edits to this file have been made by Joshua Torrance they are clearly
%%  indicated with Begin EDIT and End EDIT.  They are mainly to do with holds.
%%	Date: 11/11/2011


%%
%%  final(D,S):  program termination is possible
%%
%%  The predicate final/2 is true when program D may legally terminate
%%  in situation S.  It is typically defined recursively for higher-order
%%  program constructs.  The specifics of each individual clause are
%%  explained below.
%%

%%  It is always legal for an empty program to terminate
final(nil,_).

%%  It is never legal for a program consisting of a single action
%%  to terminate, hence there is no clause for this case.

%%  It is never legal for a program consisting of a test to terminate,
%%  hence there is no clause for this case.

%%  Sequential performance of two programs may terminate only if both
%%  programs may terminate.
final(seq(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Nondeterministic choice between two programs may terminate if either
%%  program may terminate.
final(choice(D1,D2),S) :-
    final(D1,S)
    ;
    final(D2,S).

%%  Nondeterministic choice of arguments may terminate if there is some
%%  choice of arguments for which the program will terminate.
final(pi(V,D),S) :-
    sub(V,_,D,Dr), final(Dr,S).

%%  Iteration of a program may always terminate, as it can legally be
%%  executed zero times.
final(star(_),_).

%%  Synchronised-if may terminate if the test is true and the true option
%%  may terminate, or the test is false and the false option may terminate.
final(if(Cond,D1,D2),S) :-
    holds(Cond,S), final(D1,S)
    ;
    holds(neg(Cond),S), final(D2,S).

%%  Synchronised-while may terminate if the test is false, or if the
%%  loop program may terminate.
final(while(Cond,D),S) :-
    holds(neg(Cond),S)
    ;
    final(D,S).

%%  Concurrent execution of two programs may terminate if both programs
%%  may terminate.
final(conc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Prioritised concurrent execution of two programs may terminate if both
%%  programs may terminate.
final(pconc(D1,D2),S) :-
    final(D1,S), final(D2,S).

%%  Concurrent iteration of a program may terminate in any situation, as
%%  it may be executed zero times.
final(cstar(_),_).

%%  A procedure call may terminate if the corresponding body, with arguments
%%  substituted appropriately, may terminate.
final(pcall(PArgs),S) :-
    sub(now,S,PArgs,PArgsS), proc(PArgsS,P), final(P,S).

%%  A program is also final if it contains syntactic sugar, and is equivalent
%%  to a program that is final.
final(D,S) :-
    syn_sugar(D,Ds),
    final(Ds,S).



%%
%%  trans(D,S,Dp,Sp):  program transition is possible
%%
%%  The predicate trans/4 is true when it is possible for situation S
%%  to evolve to situation Sp by executing the first part of program D.
%%  Dp is the part of D that remains to be executed in situation Sp.
%%
%%  Thus, trans/4 defines how execution of a program may be single-stepped
%%  from one situation to another.  The specifics of each individual
%%  clause are explained below.
%%

%%  It is never legal to transition an empty program, hence there
%%  is no clause for this case.

%%  A program consisting of a single action may transition by performing
%%  that action, if it's possible to do so.
trans(A,S,Dp,Sp) :-
    sub(now,S,A,AS), poss(AS,S), Sp = do(AS,S), Dp = nil.

%%  A test may transition to the empty program if it holds, leaving the
%%  situation unaltered.
trans(test(Cond),S,Dp,Sp) :-
    holds(Cond,S), S=Sp, Dp=nil.

%%  Sequential execution of two programs may transition by transitioning
%%  the first program, leaving the remainder the be executed in sequence
%%  with the second.  If the first program may terminate, it is also legal
%%  to transition the second program.
trans(seq(D1,D2),S,Dp,Sp) :-
    trans(D1,S,D1r,Sp), Dp = seq(D1r,D2).
trans(seq(D1,D2),S,Dp,Sp) :-
    final(D1,S), trans(D2,S,Dp,Sp).

%%  Nondeterministic choice of programs may transition if either of the
%%  programs may transition.
trans(choice(D1,D2),S,Dp,Sp) :-
    trans(D1,S,Dp,Sp) ; trans(D2,S,Dp,Sp).

%%  Nondeterministic choice of arguments may transition if there is an
%%  appropriate binding of the arguments for which the program may transition.
trans(pi(V,D),S,Dp,Sp) :-
    sub(V,_,D,Dr), trans(Dr,S,Dp,Sp).

%%  Iteration of a program may transition to the program followed by further
%%  iteration, provided that the program may transition.
trans(star(D),S,Dp,Sp) :-
    Dp = seq(Dr,star(D)), trans(D,S,Dr,Sp).

%%  Synchronised-if may transition if the test is true and the true option
%%  may transition, or the test is false and the false option may transition.
trans(if(Cond,D1,D2),S,Dp,Sp) :-
    holds(Cond,S), trans(D1,S,Dp,Sp)
    ;
    holds(neg(Cond),S), trans(D2,S,Dp,Sp).

%%  Syncrhonised-while may transition to the loop program in sequence
%%  with another loop, as long as the test condition holds and the loop
%%  program may transition.
trans(while(Cond,D),S,Dp,Sp) :-
    Dp = seq(Dr,while(Cond,D)), holds(Cond,S), trans(D,S,Dr,Sp).

%%  Concurrent execution may transition in two ways:
%%
%%    * Transition the first program, leaving its remainder to be
%%      executed concurrently with the second program
%%    * Transition the second program, leaving its remainder to be
%%      executed concurrently with the first program
%%
trans(conc(D1,D2),S,Dp,Sp) :-
    Dp = conc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = conc(D1,Dr2), trans(D2,S,Dr2,Sp).

%%  Prioritised concurrent execution may transition by transitioning the
%%  first program, leaving its remainder to be executed in pconc with
%%  the second program.  If it is not possible to transition the first
%%  program, then it is also legal to transition the second program leaving
%%  the first program to be executed in pconc with its remainder.
trans(pconc(D1,D2),S,Dp,Sp) :-
    Dp = pconc(Dr1,D2), trans(D1,S,Dr1,Sp)
    ;
    Dp = pconc(D1,Dr2), trans(D2,S,Dr2,Sp), \+ trans(D1,S,_,_).

%%  Concurrent iteration of a program may transition to the program
%%  being concurrently executed with more concurrent iterations of it,
%%  provided that the program can transition.
trans(cstar(D),S,Dp,Sp) :-
    Dp = conc(Dr,cstar(D)), trans(D,S,Dr,Sp).

%%  A proceduce call may transition if the body program, with arguments
%%  substituted in, may transition.
trans(pcall(PArgs),S,Dp,Sp) :-
    sub(now,S,PArgs,PArgsS),
    proc(PArgsS,P), trans(P,S,Dp,Sp).

%%  A program may also transition if it contains syntactic sugar, and is
%%  equivalent to a program that may transition.
trans(D,S,Dp,Sp) :-
    syn_sugar(D,Ds),
    trans(Ds,S,Dp,Sp).


%%
%%  syn_sugar(Din,Dout):  syntactic sugar for programs
%%
%%  This predicate is used to make writing ConGolog programs easer,
%%  by providing a level of syntax transformation into the canonical
%%  program form.  If Din is a program containing an element of this
%%  sugar, Dout is unified with the canonicalised version.
%%

%%  Sequential execution can be represented by an infix :
syn_sugar(D1 : D2,seq(D1,D2)).
%%  Nondeterministic choice can be represented by an infix /
syn_sugar(D1 / D2,choice(D1,D2)).
%%  Concurrent execution can be represented by an infix //
syn_sugar(D1 // D2,conc(D1,D2)).
%%  Prioritised concurrent execution can be represented by an infix >>
syn_sugar(D1 >> D2,pconc(D1,D2)).
%%  Condition tests can be represented by a prefix ?
syn_sugar(?C,test(C)).
%%  Procedure calls can be represented by name
syn_sugar(Proc,pcall(Proc)) :-
    proc(Proc,_).


%%
%%  holds(Cond,S):  check whether a condition holds in a situation
%%
%%  This predicate is used to evaluate reified condition terms from
%%  ConGolog programs.  It recursively reduces the formula to equivalent
%%  forms which can be tested directly by the prolog theorem prover,
%%  and hence includes the standard prolog negation-as-failure semantics.
%%  
holds(and(C1,C2),S) :-
    holds(C1,S), holds(C2,S).
holds(or(C1,C2),S) :-
    holds(C1,S) ; holds(C2,S).
holds(all(V,C),S) :-
    holds(neg(some(V,neg(C))),S).
holds(some(V,C),S) :-
    sub(V,_,C,Cr), holds(Cr,S).
holds(neg(neg(C)),S) :-
    holds(C,S).
holds(neg(and(C1,C2)),S) :-
    holds(or(neg(C1),neg(C2)),S).
holds(neg(or(C1,C2)),S) :-
    holds(and(neg(C1),neg(C2)),S).
holds(neg(all(V,C)),S) :-
    holds(some(V,neg(C)),S).
holds(neg(some(V,C)),S) :-
    \+ holds(some(V,C),S).
% Begin EDIT

% I'm not entirely sure what these predicates are supposed to be doing.
% Removing them doesn't seem to do any harm (to this program anyway) and
% it allows the holdsInSituations clauses to actually be reached.
% -joshuat 11/11/2011

%holds(P_Xs,S) :-
%    P_Xs \= and(_,_), P_Xs \= or(_,_), P_Xs \= neg(_), P_Xs \= all(_,_),
%    P_Xs \= some(_,_), sub(now,S,P_Xs,P_XsS), P_XsS.
%holds(neg(P_Xs),S) :- write('Hello!11'),
%    P_Xs \= and(_,_), P_Xs \= or(_,_), P_Xs \= neg(_), P_Xs \= all(_,_),
%    P_Xs \= some(_,_), sub(now,S,P_Xs,P_XsS), \+ P_XsS.

% NB every primitive test predicate needs a holdsInSituation clause
holds(neg(A),S) :- not(holdsInSituation(A,S)).
holds(A,S) :- holdsInSituation(A,S).

% Some simple test predicates.
% The user will need to specify holdsInSituation clauses for anything
% else they want to test.
holdsInSituation(true,_).
holdsInSituation('='(A,B),_) :- A = B.
holdsInSituation('=<'(A,B),_) :- A =< B.
holdsInSituation('=>'(A,B),_) :- A >= B.
holdsInSituation('<'(A,B),_) :- A < B.
holdsInSituation('>'(A,B),_) :- A > B.
holdsInSituation(is(A,B),_) :- A is B.

% End EDIT


%%
%%  sub(Name,Value,Old,New):  substitue values in a term
%%
%%  This predicate is true when New is equal to Old with all occurances
%%  of Name replaced by Value - basically, a symbolic substitution
%%  routine.  For example, it is usually used to produce a result such
%%  as:
%%
%%      sub(now,S,fluent(now),fluent(S)).
%%
sub(_,_,T,Tr) :-
    var(T), Tr = T.
sub(X,Y,T,Tr) :-
    \+ var(T), T = X, Tr = Y.
sub(X,Y,T,Tr) :-
    T \= X, T =.. [F|Ts], sub_list(X,Y,Ts,Trs), Tr =.. [F|Trs].

%%
%%  sub_list(Name,Value,Old,New):  value substitution in a list
%%
%%  This predicate operates as sub/4, but Old and New are lists of terms
%%  instead of single terms.  Basically, it calls sub/4 recursively on
%%  each element of the list.
%%
sub_list(_,_,[],[]).
sub_list(X,Y,[T|Ts],[Tr|Trs]) :-
    sub(X,Y,T,Tr), sub_list(X,Y,Ts,Trs).



%%
%%  trans*(D,S,Dp,Sp):  Transitive Closure of Transition Rules
%%
%%  This predicate is true if Dp,Sp are in the transitive closure of
%%  the trans/4 predicate for D,S.  It is a simplistic encoding of the
%%  transitive closure in prolog using a recursive definition.
%%
trans*(D,S,D,S).
trans*(D,S,Dp,Sp) :-
    trans(D,S,Dr,Sr),
    trans*(Dr,Sr,Dp,Sp).


%%
%%  step(D,S,Dp,Sp):  single-step a program
%%
%%  This predicate takes a program D and a situation S in which to execute it,
%%  and returns a new situation Sp and remaining program Dp such that the
%%  execution of D has progressed by a single action.  It may be used
%%  repeatedly to find a possible next action to perform for a given program.
%%
step(D,S,Dp,Sp) :-
    %%  Naive implementation is simply:  trans*(D,S,Dp,do(C,T,S))
    %%  This implementation is more efficient as it does not generate
    %%  transitions that go beyond one action from S (which will always fail).
    Sp = do(_,S), trans(D,S,Dp,Sp)
    ;
    trans(D,S,Dr,S), step(Dr,S,Dp,Sp).


%%
%%  do(D,S,Sp):  offline execution of ConGolog Programs
%%
%%  This predicate takes a program D and starting situation S, and
%%  finds a new situation Sp which can be legally reached by executing
%%  program D to termination.  The actions executed can be retreived
%%  from the situation Sp.
%%
%%  This predicate is capable of backtracking over action choices to
%%  find a legal execution, and so is not suitable for executing programs
%%  on-line.
%%
do(D,S,Sp) :-
    trans*(D,S,Dp,Sp),
    final(Dp,Sp).

%%  Predicate for printing the action history of a situation.
show_action_history(s0) :-
    nl.
show_action_history(do(A,S)) :-
    show_action_history(S),
    display('do '), display(A), nl.

% Begin EDIT
%%
%%  sit_len(S,Len): length of a situation
%%
sit_len(s0,0).
sit_len(do(_,S),N) :-
	sit_len(S,M), N is M+1.
% End EDIT
