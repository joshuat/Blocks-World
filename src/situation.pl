%%
%%  Initial Conditions for the domain
%%
%%  The initial conditions are specified by additional clauses for
%%  each fluent, with the situation term set to the atom s0.  For
%%  the most part no fluents hold in the initial situation, so 
%%  there arent many clauses here.
%%

on_top(u, a, s0).
on_top(a, b, s0).
on_top(b, c, s0).     on_top(v, e, s0).
on_top(c, d, s0).     on_top(e, f, s0).     on_top(w, g, s0).
on_top(d, floor, s0). on_top(f, floor, s0). on_top(g, floor, s0).

%%
%%  Entities in the domain.
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
strength(r1, 1).
strength(r2, 2).
strength(r3, 3).


%%
%%  height(Robot, Height): specify the maximum height (in blocks)
%%  						that a robot can stack
%%
height(r1, 10).
height(r2, 5).
height(r3, 4).


%%  
%%  block(Block):  specify blocks in the system
%%
%%  This predicate is true when Block is the name of a block in the world.
%%
block(a).
block(b).
block(c).
block(d).
block(e).
block(f).
block(g).
block(u).
block(v).
block(w).


%%
%%  weight(Block, Weight): specify the weight of each block
%%
weight(a, 1).
weight(b, 1).
weight(c, 1).
weight(d, 1).
weight(e, 1).
weight(f, 1).
weight(g, 1).
weight(u, 1).
weight(v, 1).
weight(w, 1).

%%
%%  type(Block, Type): specify the type of each block
%%
%%  eg. Blocks could be products so, type(block,productA).
%%
type(a, a).
type(b, a).
type(c, a).
type(d, b).
type(e, b).
type(f, b).
type(g, c).
type(u, c).
type(v, c).
type(w, d).

%%  
%%  floor(Floor): specifies the floor
%%
floor(floor).

