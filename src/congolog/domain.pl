%%
%%  blocksdomain.pl:  Axiomatisation of an extended blocks world
%%					  domain for use with ConGolog.
%%
%%  Author:  Joshua Torrance (joshuat)
%%  Date:  11/11/2011
%%
%%  This file contains an axiomatisation of an extended blocks world domain
%%  in the situation calculus. This file has been adapted from Ryan Kelly's
%%  Cooking Agents domain.
%%
%%  Specifics about the domain (such an initial situation and entities) are
%%  detailed in situation.pl.
%%
%%  Extentions to blocks world:
%%  -Weight of blocks and strength of robots.
%%  -Height of robots.
%%  -Sensible choice of actions (to a fairly limited degree at the moment).
%%
%%  TODO: Make action selection more efficient. See good_action.
%%  TODO: Implement true concurrency.
%%


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
%%  move(Robot,Block,Place): Robot moves Block to Place.
%%
primitive_action(move(Robot,Block,Place)) :-
    robot(Robot), block(Block),
    (block(Place) ; floor(Place)).


%%
%%  noop(Agent): Agent does nothing.
%%
%%  As the action as no effect, successor state axioms are not necessary.
%%
primitive_action(noop(Agent)) :-
	agent(Agent).
	

%%
%%  poss(A,S):  possibility of performing an action
%%
%%  This predicate is true when it is possible to perform action
%%  A in situation S.
%%

%%
%%  move(Robot,Block,Place)
%%
%%  -blocks can't have anything on top of it
%%  -blocks can't be moved to a block they're already on
%%  -place can't have anything on top of it unless it's the floor
%%  -blocks cannot be placed above a robots height
%%  -robots can't move blocks heavier than their strength
poss(move(Robot,Block,Place),S) :-
    Block \= Place,
	\+ on_top(_,Block,S),
	\+ on_top(Block,Place,S),
	height(Place, PHeight, S), height(Robot, RHeight), RHeight > PHeight,
	weight(Block,W), strength(Robot,St), W =< St,
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
%%  If the fluent is to be tested (ie. ?(fluent(Args))) then it needs
%%  holdsInSituation to be defined for it.
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

holdsInSituation(on_top(X,Y),S) :- on_top(X,Y,S).

%%
%%  height(Block,Height,S): block is at Height in S.
%%
%%  This fluent is true when block is at the given height in
%%  the situation. A block on the floor is at height 1, a block
%%  on such a block is at height 2, etc.
%%
%%  This is a pseudo-fluent. It depends on other fluents only so
%%  it doesn't require a successor state axiom.
%%
height(Floor,0,_) :- floor(Floor).
height(Block,Height,S) :-
    on_top(Block,Y,S), height(Y,H,S), Height is H+1.

%%
%%  good_action(A,S): A is a 'good' action in S
%%
%%  good_action is used to choose sensible, intelligent and
%%  hopefully efficient actions in order to speed up the execution
%%  of the program.
good_action(A,S) :- not(bad_action(A,S)).


%%
%%  bad_action(A,S): A is a 'bad' action in S
%%
%%  These are actions that do not contribute towards the goal.

% Move the same block twice in a row. (this would be fine with concurrency)
bad_action(move(_,A,_),do(move(_,A,_),_)).
