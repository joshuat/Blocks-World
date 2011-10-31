goal(S) :-
    onTop(block1, block2, S), legal(S).

proc(stackBoxes(Robot),
    getStacking(Robot)
    ).
proc(stackBoxes([Robot|Robots]),
    getStacking(Robot) // stackBoxes(Robots)
    ).


proc(getStacking(Robot),
    pi(b1, ?Block(b1)
            : pi(b2, ?(Block(b2), b1\=b2))
            : stack(Robot, b1, b2)
	  )
    : getStacking(Robot).
    ).


proc(stack(Robot, Block1, Block2),
    pick_up(Robot, Block1) : put_down(Robot, Block1, Block2)
	).

