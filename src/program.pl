:- consult(golog).
:- include(domain).
:- include(situation).

proc(stack(N,Limit),
    if(?(goal),
        true, % Do nothing
        if(?(N>Limit),
            false,
            pi(robot,
                pi(block,
                    pi(destination,
                        move(robot,block,destination) :
                            pi(m, ?(m is N+1) : stack(m, Limit))
                    )
                )
            )
        )
    )
).

proc(print, ?(printer) : ?(printer)).



holdsInSituation('='(A,B),_) :- A = B.
holdsInSituation('=<'(A,B),_) :- A =< B.
holdsInSituation(is(A,B),_) :- A is B.
holdsInSituation(goal,S) :- goal(S).
holdsInSituation(on_top(X,Y),S) :- on_top(X,Y,S).
holdsInSituation(printer,_) :- write('Hello').
