
Attached is a version of ConGolog for SWI and the cooking domain
written by Ryan Kelly.

There are five files:
  sitcalc.pl  is some basic sitcalc predicates
  congolog.pl  is the ConGolog semantics
  domain.pl   is the axiomatisation of the cooking domain
  program.pl   is some example program definitions
  main.pl     is the main entry point.

To run it,  start SWI Prolog and load main.pl 
- by double clicking on the 'main.pl' file from windows explorer 
(if using Windows)
- or by issuing the following command (if using console version)
?-consult(main). 

Note: main.pl loads in all of the files 
sitcalc.pl, congolog.pl, domain.pl and program.pl.

Then execute the main/0 predicate
?-main(X).

It will give you a printout of the actions performed by the agent, 
and the resulting dishes produced for dinner.

