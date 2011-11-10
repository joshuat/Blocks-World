%%
%%  Initial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%

on_top(a, b, s0).       on_top(d, f, s0).
on_top(b, floor, s0).   on_top(f, floor, s0).



%%
%%  Entities in the domain.
%%

%%  
%%  robot(Rob):  specify robots (agents) in the system
%%
%%  This predicate is true when Rob is the name of a robot in the world.
%%
robot(r1).
%robot(r2).
%robot(r3).


%%
%%  strength(Robot, Strength): specify the strength of each robot
%%
strength(r1, 1).
strength(r2, 2).
strength(r3, 3).


%%
%%  height(Robot, Height): specify the maximum height (in blocks)
%%  						that a robot can stack
%%
height(r1, 3).
height(r2, 5).
height(r3, 4).


%%  
%%  block(Block):  specify blocks in the system
%%
%%  This predicate is true when Block is the name of a block in the world.
%%
block(a).
block(b).
%block(c).
block(d).
%block(e).
block(f).
%block(g).


%%
%%  weight(Block, Weight): specifes the weight of each block
%%
weight(a, 2).
weight(b, 1).
weight(c, 3).
weight(d, 4).
weight(e, 5).
weight(f, 6).
weight(g, 4).


%%
%%  type(Type): specifies the types in the system
%%
type(t1).
type(t2).
type(t3).

%%
%%  type(Block, Type): specify the type of each block
%%
%%  eg. Blocks could be products so, type(block,productA).
%%
type(a, t1).
type(b, t1).
type(c, t1).
type(d, t2).
type(e, t2).
type(f, t2).
type(g, t3).

%%  
%%  floor(Floor): specifies the floor
%%
floor(floor).

