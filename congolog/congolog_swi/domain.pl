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
%robot(robot2). Let's start with just one robot.
%robot(robot3).


%%
%%  prim_obj(Obj,Type):  specify primitive objects in the world
%%
%%  This predicate is true when Obj is the name of a primitive object
%%  in the world, of type Type.
%%
%%  prim_obj(Obj):  shortcut to check object names
%%
%%  This predicate is true if Obj is the name of a primite object,
%%  regardless of its type.
%%

prim_obj(Obj) :-
    prim_obj(Obj,_).

% I don't think I need these.
%prim_obj(Obj,block) :-
%    member(Obj,[block1,block2,block3]).
%prim_obj(Obj,robot) :-
%    member(Obj,[robot1,robot2,robot3]).
%prim_obj(Obj,stack) :-
%    member(Obj,[stack1,stack2,stack3]).


%%
%%  super_type(SubType,SuperType):  specify type hierarchy
%%
%%  This predicate is true when all objects of type SubType are
%%  also of type SuperType.
%%  
% No super types yet
%super_type(Type,container) :-
%    member(Type,[bowl,board,oven]).
%super_type(Type,ingredient) :-
%    member(Type,[flour,egg,tomato,lettuce,sugar]).

%%
%%  obj_is_type(Obj,Type):  check object types
%%
%%  This predicate is true when the object named Obj is of type
%%  Type according to the hierarchy of super-types.
%%
obj_is_type(Obj,Type) :-
    prim_obj(Obj,Type)
    ;
    super_type(SubType,Type), obj_is_type(Obj,SubType).


%%
%%  prim_action(Act):  specify primitive actions
%%
%%  This predicate is true when Act is the name of a primitive action
%%  in the world.  Actions are typically parameterised in terms of the
%%  objects they act on.  See the details of the ConGolog situation
%%  calculus for further information.
%%

%%  pick_up(Robot,Block): Robot picks up a block.
prim_action(pick_up(Robot,Block)) :-
    robot(Robot), block(Block).

%%  put_down(Robot,Block,Place):  Robot puts Block on Place.
prim_action(put_down(Robot,Block,Place)) :-
    robot(Robot), block(Block), (block(Place) ; stack(Place)).


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

%%  Robots can only put blocks on top of blocks/stacks that
%%  don't have something on top of them already and if they're
%%  holding that block.
poss(put_down(Robot, Block, Place),S) :-
    holding(Robot, Block, S), \+ onTop(_, Place, S).


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

********************************************************************
*********************** I'M UP TO HERE!!!!!!!!! ********************
********************************************************************

%%
%%  holding(Robot,Block,S): robot is holding the block
%%
%%  This fluent is true when the robot is holding the block in
%%  situation S. It becomes true if the robot picks up the block
%%  and becomes false if the robot puts the block down.
%%
holding(Robot,Block,do(A,S)) :-
    A = pick_up(Robot,Block)
	;
	holding(Robot,Block,S),
	\+ (a=put_down(Robot,Block,_)).

%%
%%  on_top(Block,Y,S): block is on top of y
%%
%%  This fluent is true when the block is on top of y in
%%  situation S. It becomes true when the block is put on
%%  top of y and it becomes false if the block is picked up.
%%
on_top(Block,Y,do(A,S)) :=
    A = put_down(_,Block,Y)
	;
	on_top(Block,Y,S),
	\+ (a=pick_up(_,Block)).

%%
%%  history_length(N,S):  length of the action histoy in a situation
%%
%%  This simple fluent encodes in N the number of actions that have
%%  taken place in the history of situation S.  It is used to make this
%%  information easily available to agents.
%%
history_length(N,do(_,S)) :-
    history_length(N1,S),
    N is N1 + 1.
history_length(0,s0).

%%
%%  Intial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%

% initially, nothing is true...

