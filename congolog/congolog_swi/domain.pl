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
%%    different types (indicated by prim_object/2) which in turn may
%%    be part of super-types (indicated by super_type/2).
%%


%%  
%%  robot(Rob):  specify robots (agents) in the system
%%
%%  This predicate is true when Rob is the name of a robot in the world.
%%
robot(robot1).
robot(robot2).
%robot(robot3).

agent_list([robot1, robot2]).
%%
%%  Robots are agents.
%%
agent(Robot) :-
	robot(Robot).


%%  
%%  block(Block):  specify blocks in the system
%%
%%  This predicate is true when Block is the name of a block in the world.
%%
block(block1).
block(block2).
%block(block3).


%%  
%%  floor(Floor): specifies the floor
%%
floor(floor).


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

%%  pick_up(Robot,Block): Robot picks up a block.
primitive_action(pick_up(Robot,Block)) :-
    robot(Robot), block(Block).

%%  put_down(Robot,Block,Place):  Robot puts Block on Place.
primitive_action(put_down(Robot,Block,Place)) :-
    robot(Robot), block(Block), ( block(Place) ; floor(Place) ).


%%
%%  poss(A,S):  possibility of performing an action
%%
%%  This predicate is true when it is possible to perform action
%%  A in situation S.
%%

%%  Robots can only pick up a block if no robot is holding
%%  that block and if that robot is not holding a block.
poss(pick_up(Robot,Block),S) :-
    \+ holding(_,Block,S), \+ holding(Robot,_,S).

%%  Robots can only put blocks on top of blocks that
%%  don't have something on top of them already and if they're
%%  holding that block. Blocks can also be placed on the gound.
poss(put_down(Robot, Block, Place),S) :-
    Block \= Place,
	holding(Robot, Block, S),
	(
		\+ on_top(_, Place, S),
		block(Place)
		;
		floor(Place)
	).

%%  It is always possible to do nothing.
poss(noop(Robot), _) :-
	robot(Robot).

%%
%%  Similar actions.
%%
%%  With multiple agents we need to prevent them from performing
%%  the same actions at the same time. This is done by defining
%%  similar_action/3.
%%
similar_action(pick_up(_, Block), pick_up(_, Block)).
similar_action(put_down(_, Block, _), put_down(_, Block, _)).
similar_action(put_down(_, BlockA, BlockB), put_down(_, BlockB, BlockA)).


%%
%%  Fluents in the Domain
%%
%%  The fluents are specified in terms of their successor state axioms,
%%  of the form "a fluent is true if it became true, or was previously
%%  true and did not become false".
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
%%  holding(Robot,Block,S): robot is holding the block
%%
%%  This fluent is true when the robot is holding the
%%  block in situation S. It becomes true if the robot
%%  picks up the block and it becomes false if the robot
%%  puts the block down.
%%
holding(Robot,Block,do(A,S)) :-
    A = pick_up(Robot,Block)
	;
	holding(Robot,Block,S),
	\+ (A=put_down(Robot,Block,_)).


%%
%%  on_top(Block,Y,S): block is on top of Y
%%
%%  This fluent is true when the block is on top of Y in
%%  situation S. It becomes true when the block is put on
%%  top of Y and it becomes false if the block is picked up.
%%
on_top(Block,Y,do(A,S)) :-
    A=put_down(_,Block,Y)
	;
	on_top(Block,Y,S),
	\+ (A=pick_up(_,Block)).


%%
%%  Intial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%

on_top(block1, floor, s0).
on_top(block2, floor, s0).
on_top(block3, floor, s0).
