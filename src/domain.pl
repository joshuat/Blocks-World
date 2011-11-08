%%
%%  blocksdomain.pl:  Axiomatisation of the "Blocks World" domain for ConGolog
%%
%%  Author:  Joshua Torrance (joshuat)
%%
%%  Date Created:  06/10/11
%%
%%    This file will contain an axiomatisation of the "Blocks World" domain
%%    in the situation calculus. This file has been adapted from Ryan Kelly's
%%    Cooking Agents domain.
%%
%%    The domain consists of several agents and inanimate objects of
%%    different types (indicated by prim_object/2).
%%

%%  The domains entities and initial situation are described in situation.pl


%%
%%  Robots are agents.
%%
agent(Robot) :-
	robot(Robot).


%%
%%  primitive_action(Act):  specify primitive actions
%%
%%  This predicate is true when Act is the name of a primitive action
%%  in the world.  Actions are typically parameterised in terms of the
%%  objects they act on.

%%
%%  The following below the "no-op" action.  As the action as no effect,
%%  successor state axioms are not necessary.
%%
primitive_action(noop(A)) :-
    agent(A).

%%  move(Robot,Block,Place): Robot moves Block to Place.
primitive_action(move(Robot,Block,Place)) :-
    robot(Robot), block(Block),
    (block(Place) ; floor(Place)).


%%
%%  poss(A,S):  possibility of performing an action
%%
%%  This predicate is true when it is possible to perform action
%%  A in situation S.
%%

%%
%%  move(Robot,Block,Place)
%%
%%  Robots can only move blocks on top of blocks that don't
%%  have something on top of them already (unless they're
%%  putting the block on the floor). Blocks cannot be placed
%%  above a robot's max height.
poss(move(Robot,Block,Place),S) :-
    Block \= Place,
	height(Place, PlaceHeight, S), height(Robot, RobotHeight),
    RobotHeight > PlaceHeight,
	(
		\+ on_top(_,Place,S),
		block(Place)
		;
		floor(Place)
	).

%%  It is always possible to do nothing.
poss(noop(_), _).


%%
%%  Fluents in the Domain
%%
%%  The fluents are specified in terms of their successor state axioms,
%%  of the form :
%%  	A fluent is true if it became true, or was previously true and
%%  	did not become false.
%%
%%    fluent_holds(Args,do(A,S)) :-
%%        fluent_becomes_true(Args,do(A,S))
%%        ;
%%        (
%%          fluent_holds(Args,S),
%%          \+ fluent_becomes_false(Args,do(A,S))
%%        )
%%

%%
%%  on_top(Block,Y,S): block is on top of Y
%%
%%  This fluent is true when the block is on top of Y in
%%  situation S. It becomes true when the block is put on
%%  top of Y and it becomes false if the block is picked up.
%%
on_top(Block,Y,do(A,S)) :-
    A=move(_,Block,Y)
	;
	on_top(Block,Y,S),
	\+ (A=move(_,Block,_)). % It isn't possible to move Block onto Y
                            % so we don't need to check the 3rd arg.


%%
%%  height(Block,Height,S): block is at Height in S.
%%
%%  This fluent is true when block is at the given height in
%%  the situation. A block on the floor is at height 1, a block
%%  on such a block is at height 2, etc.
%%
height(floor,0,_).
height(Block,Height,S) :-
    on_top(Block,Y,S), height(Y,Height-1,S).


