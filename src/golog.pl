%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                A GOLOG INTERPRETER IN SWI-PROLOG
%
% Adapted from Sardina's version by Edward Stabler. Most notably:
%     * I separate situation-specific "holds" requirements
%       from the Lloyd-Topor analysis. Then instead of calling
%       "restoreSitArgs(P,S,F)" and then calling F, I use:
%             "holdsInSituation(P,S) :- F."
%       NB: Every predicate that can appear in ?(_) -- every "fluent" --
%           needs to have a holdsInSituation clause.
%
% Adapted from the ECLIPSE version by Sebastian Sardina.
%
%                             Aug 2009
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.  
%
%       Do not distribute without permission.
%       Include this notice in any copy made.
%
%        Copyright (c) 1992-1997 by The University of Toronto,
%                       Toronto, Ontario, Canada.
%                         All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for research purpose is hereby granted without fee,
% provided that the above copyright notice appears in all copies and
% that both the copyright notice and this permission notice appear in
% supporting documentation, and that the name of The University of
% Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations
% about the suitability of this software for any purpose.  It is
% provided "as is" without express or implied warranty.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(optimise, true).
:- op(800, xfy, [&]).   % Conjunction
:- op(850, xfy, [v]).   % Disjunction
:- op(870, xfy, [=>]).  % Implication
:- op(880,xfy, [<=>]).  % Equivalence
:- op(950, xfy, [:]).   % Action sequence
:- op(960, xfy, [#]).   % Nondeterministic action choice
 
do(E1 : E2,S,S1) :- do(E1,S,S2), do(E2,S2,S1).
do(?(P),S,S) :- holds(P,S).
do(E1 # E2,S,S1) :- do(E1,S,S1) ; do(E2,S,S1).
do(if(P,E1,E2),S,S1) :- do((?(P) : E1) # (?(-P) : E2),S,S1).
do(star(E),S,S1) :- S1=S ; do(E : star(E),S,S1).
do(while(P,E),S,S1):- do(star(?(P) : E) : ?(-P),S,S1).
do(pi(V,E),S,S1) :- sub(V,_,E,E1), do(E1,S,S1).
do(E,S,S1) :- proc(E,E1), do(E1,S,S1).
do(E,S,do(E,S)) :- primitive_action(E), poss(E,S).

% sub(Name,New,Term,Term2): Term2 is Term with Name replaced by New
sub(_,_,T,T2) :- var(T), T2=T.
sub(X,X2,T,T2) :- \+ var(T), T=X, T2=X2.
sub(X,X2,T,T2) :- \+ T=X, T=..[F|L], sub_list(X,X2,L,L2), T2=..[F|L2].
sub_list(_,_,[],[]).
sub_list(X,X2,[T|L],[T2|L2]) :- sub(X,X2,T,T2), sub_list(X,X2,L,L2).

% The holds predicate implements the revised Lloyd-Topor
%   transformations on test conditions.
holds(P & Q,S) :- holds(P,S), holds(Q,S).
holds(P v Q,S) :- holds(P,S); holds(Q,S).
holds(P => Q,S) :- holds(-P v Q,S).
holds(P <=> Q,S) :- holds((P => Q) & (Q => P),S).
holds(-(-P),S) :- holds(P,S).
holds(-(P & Q),S) :- holds(-P v -Q,S).
holds(-(P v Q),S) :- holds(-P & -Q,S).
holds(-(P => Q),S) :- holds(-(-P v Q),S).
holds(-(P <=> Q),S) :- holds(-((P => Q) & (Q => P)),S).
holds(-all(V,P),S) :- holds(some(V,-P),S).
holds(-some(V,P),S) :- \+ holds(some(V,P),S). % Negation by failure
holds(-P,S) :- isAtom(P), \+ holds(P,S).      % Negation by failure
holds(all(V,P),S) :- holds(-some(V,-P),S).
holds(some(V,P),S) :- sub(V,_,P,P1), holds(P1,S).

% NB every primitive test predicate needs a holdsInSituation clause
holds(A,S) :- holdsInSituation(A,S).

isAtom(A) :- \+ (A=(-W) ; A=(W1 & W2) ; A=(W1 => W2) ;
    A=(W1 <=> W2) ; A=(W1 v W2) ; A=some(X,W) ; A=all(X,W)).
