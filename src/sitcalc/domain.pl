%%
%%  blocksdomain.pl:  Axiomatisation of the "Blocks World" domain for ConGolog
%%
%%  Author: Joshua Torrance (joshuat)
%%  Date:  11/11/2011
%%
%%  This file contains an axiomatisation of the blocks world domain
%%  in the situation calculus. This file has been adapted from Ryan Kelly's
%%  Cooking Agents domain.
%%
%%  TODO: Move scenario specific information (entities, s0, etc.) to a
%%			separate file.
%%


%%  
%%  robot(Rob):  specify robots (agents) in the system
%%
%%  This predicate is true when Rob is the name of a robot in the world.
%%
robot(robot1).
robot(robot2).
robot(robot3).


%%
%%  strength(Robot, Strength): specify the strength of each robot
%%
strength(robot1, 1).
strength(robot2, 1).
strength(robot3, 1).


%%
%%  height(Robot, Height): specify the maximum height (in blocks)
%%  						that a robot can stack
%%
height(robot1, 2).
height(robot2, 2).
height(robot3, 2).


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
block(a).
block(b).
block(c).
block(d).


%%
%%  weight(Block, Weight): specify the weight of each block
%%
weight(a, 2).
weight(b, 1).
weight(c, 1).
weight(d, 1).


%%
%%  type(Block, Type): specify the type of each block
%%
%%  eg. Blocks could be products so, type(block,productA).
%%
type(a, type1).
type(b, type1).
type(c, type2).
type(d, type2).


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
%%  The noop action has been removed to reduce clutter and
%%  speed up execution times.
%%
%primitive_action(noop(A)) :-
%    agent(A).

%% grab(Robot,Block): Robot grabs a block.
primitive_action(grab(Robot,Block)) :-
	robot(Robot), block(Block).

%% let_go(Robot,Block): Robot lets go of a grabbed block.
primitive_action(let_go(Robot,Block)) :-
	robot(Robot), block(Block).

%%  lift(Robot,Block): Robot lifts up a grabbed block.
primitive_action(lift(Robot,Block)) :-
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

%%  Robots can only grab one block at a time.
poss(grab(Robot,_),S) :-
	\+ holding(Robot,_,S).

%%  Robots can only let go of blocks they are holding and only if
%%  the block is not being lifted.
poss(let_go(Robot,Block),S) :-
	holding(Robot,Block,S), \+ lifted(Block,S).

%%  Robots can only lift up a block if they are holding that
%%  block, it hasn't already been lift and it doesn't have anything
%%  on top of it.
%%  Strength and weight are handled in the simultaneous action rules.
poss(lift(Robot,Block),S) :-
    holding(Robot,Block,S), \+lifted(Block,S), \+on_top(_,Block,S).

%%  Robots can only put blocks on top of blocks that
%%  don't have something on top of them already and aren't lifted,
%%  if they're holding that block and it has been lifted.
%%  Blocks cannot be placed above a robot's max height.
%%  Blocks can also be placed on the floor.
poss(put_down(Robot,Block,Place),S) :-
    Block \= Place,
	lifted(Block,S),
	holding(Robot,Block,S),
	\+ lifted(Place,S),
	height(Place, BH, S), height(Robot, RH), RH > BH,
	(
		\+ on_top(_,Place,S),
		block(Place)
		;
		floor(Place)
	).

%%  It is always possible to do nothing.
%%  Caveat: To reduce uninteresting permutations
%%  		it is not possible to do nothing while holding a block.
poss(noop(Robot), S) :-
	robot(Robot), \+holding(Robot,_,S).

	
%%
%%  Simultaneous action rules.
%%  simultaneous_action(GroupAction, S)
%%
%%  With multiple agents we need to prevent some actions from
%%  occurring at the same time. Actions defined by simultaneous_action
%%  are NOT allowed.
%%
%%  Robots must be strong enough to lift blocks.
simultaneous_action(Actions,_) :-
	member(A,Actions), A=lift(_,Block),
	total_lift_strength(Block,Actions,St),
	weight(Block,Weight),
	St < Weight.		% Remember it's negated.
%%  All robots holding a block must put it down together.
simultaneous_action(Actions,S) :-
	member(A,Actions), A=put_down(RobotA,Block,_),
	holding(RobotB,Block,S), RobotB\=RobotA,
	member(B,Actions), actor(B,RobotB), B\=put_down(RobotB,Block,_).
%%  All robots putting a block down must put it in the same place.
simultaneous_action(Actions,_) :-
	member(A,Actions), A=put_down(_,Block,PlaceA),
	member(B,Actions), B=put_down(_,Block,PlaceB),
	PlaceA\=PlaceB.
%%  Cannot put A on B and B on A at the same time.
simultaneous_action(Actions,_) :-
	member(A,Actions), A=put_down(_,BlockA,BlockB),
	member(B,Actions), B=put_down(_,BlockB,BlockA).
%%  Robots cannot put blocks down on blocks that are being lifted.
simultaneous_action(Actions,_) :-
	member(A,Actions), A=put_down(_,_,Place),
	member(B,Actions), B=lift(_,Place).
%%  Cannot put two different blocks in the same place (unless floor).
simultaneous_action(Actions,_) :-
	member(A,Actions), A=put_down(_,BlockA,BlockC),
	member(B,Actions), B=put_down(_,BlockB,BlockC),
    BlockA\=BlockB, BlockC\=floor.
%%  Robots cannot lift a block and put it down at the same time.
simultaneous_action(Actions,_) :-
	member(A,Actions), A=put_down(_,Block,_),
	member(B,Actions), B=lift(_,Block).
%%  In an effort to clean up let's not allow all actions to be noop.
simultaneous_action(Actions,_) :-
	all_noop(Actions).


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
%%  holding(Robot,Block,S): robot is holding the block
%%
%%  This fluent is true when a robot is holding the block.
%%  It becomes true if it is grabbed and it becomes false if
%%  it is let go.
holding(Robot,Block,do(A,S)) :-
	A = grab(Robot,Block)
	;
	holding(Robot,Block,S),
	\+ (A=let_go(Robot,Block)).


%%
%%  lifted(Block,S): robots have lifted the block up
%%
%%  This fluent is true when robots have lifted the
%%  block in situation S. It becomes true if it is lifted and
%%  it becomes false if it is put down.
%%
lifted(Block,do(A,S)) :-
    A = lift(_,Block)
	;
	lifted(Block,S),
	\+ (A=put_down(_,Block,_)).


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
	\+ (A=lift(_,Block)).


%%
%%  Initial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%

on_top(a, b, s0).
on_top(b, floor, s0).	on_top(c, floor, s0). on_top(d, floor, s0).


%%
%%  Domain related utility predicates.
%%

%%
%%  total_lift_strength(Block,Actions,Strength): Gives the total strength lifting
%%  	Block in Actions.
%%
total_lift_strength(_,[],0).
total_lift_strength(Block,[lift(Robot,Block)|As],Strength) :-
	total_lift_strength(Block,As,St),
	strength(Robot,RobotStrength),
	Strength is St + RobotStrength.
total_lift_strength(Block,[A|As],Strength) :-
	\+(A=lift(_,Block)),
	total_lift_strength(Block,As,Strength).

	
%%
%%  all_noop(GroupAction): all the actions are noop(_)
%%
all_noop([]).
all_noop([noop(_)|As]) :-
	all_noop(As).

	
%%
%%  height(Block, Height, S): gives the height of a block
%%
%%  eg. A block on the floor will have a height of 1.
%%      A block on that block will have a height of 2.
height(floor,0,_).
height(Block,Height,S) :-
	block(Block),
	on_top(Block,Base,S),
	height(Base,BaseHeight,S),
	Height is BaseHeight+1.


%%
%%  primary_object(Action, Object): gives the first object for the action.
%%
primary_object(Action, Object) :-
    primitive_action(Action), arg(2,Action,Object).
