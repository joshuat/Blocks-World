%%
%%  main.pl:  Top-level prolog file for ConGolog implementation
%%
%%  Author:  Ryan Kelly (rfk)
%%
%%  Date Created:  12/03/07
%%
%%    This file is the entry-point for a ConGolog program consisting
%%    of the following files:
%%
%%      * Axioms of the situation calculus, in sitcalc.pl
%%      * The ConGolog semantics, from congolog.pl
%%      * A domain axiomatisation, from domain.pl
%%
%%    It imports the necessary prolog libraries and performs other
%%    initialisation tasks.  It also provides the predicate main/1
%%    which may be called to execute the ConGolog procedure named
%%    'control'.
%%

%:- discontiguous trans/4, final/2, prim_action/1, natural/1, poss/3,
%                 conflicts/3, start/2.

%%
%%  Include the relevant definitions
%%
:- include(utility).
:- include(sitcalc).
:- include(domain).
:- include(goals).
:- include(program).
