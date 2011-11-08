goal1(S) :-
    on_top(u, v, S),
    on_top(v, w, S),
    on_top(w, d, S),
    on_top(d, c, S),
    on_top(c, b, S),     on_top(g, f, S),
    on_top(b, a, S),     on_top(f, e, S),
    on_top(a, floor, S), on_top(e, floor, S).

goal(S) :-
    on_top(a, b, S),     on_top(u, v, S),
    on_top(b, c, S),     on_top(v, e, S),
    on_top(c, d, S),     on_top(e, f, S),     on_top(w, g, S),
    on_top(d, floor, S), on_top(f, floor, S), on_top(g, floor, S).


