goal(S) :-
    on_top(block1, block2, S).

dynamic_goal(S) :-
    not(lifted(_,S)),
    stack_list(Stacks, S),
    ideal_num_stacks(NStacks), length(Stacks,NStacks),
    heaviest_on_bottom(Stacks).



%%
%%  Goal Utility Predicates
%%

%%
%%  stack_list(StackList, S): gives a list of all stacks
%%
%%  eg. for the situation:
%%      a  b
%%      c  d  e
%%      f  g  i
%%      floor
%%
%%  StackList would be [[f,c,a],[g,d,b],[i,e]]
%%
stack_list(Stacks,S) :-
    findall(B,on_top(B,floor,S),BFloor),
    stack_list2(BFloor, Stacks, S).

stack_list2([],[],_).
stack_list2([F|Fs],[S|Ss],Sit) :-
    get_stack(F,S,Sit), stack_list2(Fs,Ss,Sit).

%%
%%  get_stack(Block, Stack, S): gets the list of blocks on top of
%%      Block in S.
%%
get_stack(Block,Stack,S) :-
    on_top(Above,Block,S),
    get_stack(Above,AStack,S),
    Stack = [Block|AStack]
    ;
    not(on_top(_,Block,S)),
    Stack = [Block].


%%
%%  heaviest_on_bottom(Stacks): true if the blocks with the biggest
%%      weight are below those with less.
%%
%%  Note: Don't need the situation here because weight doesn't change.
heaviest_on_bottom([S]) :-
    heaviest_on_bottom2(S).
heaviest_on_bottom([S|Ss]) :-
    heaviest_on_bottom2(S), heaviest_on_bottom(Ss).

heaviest_on_bottom2([_]).
heaviest_on_bottom2([B1|Bs]) :-
    head(Bs,B2),
    weight(B1,W1), weight(B2,W2), W1>=W2,
    heaviest_on_bottom2(Bs).


%%
%%  max_height(Height): gives the maximum height of a stack
%%
%%  ASSUMPTION: the heaviest block can be lifted to the highest height.
%%  It'd be nice to get rid of this assumption without a horrendously
%%  complicated predicate.
%%
max_height(HMax) :-
    findall(R,robot(R),RList), map(height,RList,HList), max(HList,HMax).


%%
%%  num_blocks(N): gives the number of blocks in the domain
%%
num_blocks(N) :-
    findall(B,block(B),BList), length(BList,N).


%%
%% ideal_num_stacks(N): gives the ideal (lowest) number of stacks
%%
ideal_num_stacks(N) :-
    max_height(H), num_blocks(B),
    N is ceiling(B / H).
