# Goal Regression Planner With Logical Consequences

There are **two directories** in the project. **en** contains the project with the **english** documentation, **it** in italian.

To obtain the logical consequences, we declare as dynamically true the predicates that are passed through the `S2` list.
We keep a pointer called `list` to `S2`, in order to be able to modify its variables at runtime, which will be able to unify with those of `S1`, in the event that all the goals of `S1` are a logical consequence of `S2`.

```PROLOG
logical_consequence(S1,S2, DeleteList) :-
  nb_linkval(list, S2), % we need nb_linkval to keep a pointer to the list that contains new clauses
  instanciate([S2],DeleteList),
  see(S1, false).
```

`DeleteList` is used to declare NOT(predicate) as true. 

Example:  **[plays(andrea, basket), not(plays(marco, basket))]**

By means of the not predicate it can be stated that something is NOT true. You can create recursive NOT predicates: for example `not(not(not(something)))` will be interpreted as `not(something)`.

`instanciate/2` asserts the predicates of `S2` as true.

```prolog
states([T|C], DeleteList):-
  recursiveNot(T, NT),
  asserta((v(NT) :- v(NT,false))),
  ((NT=not(X),!;DeleteList=true,X=NT),!, % if we have to deny the claim
   asserta((v(X):- v(X,false))),
   asserta(v(not(X),false)),   % we affirm that v(not(State)) is true
    asserta((v(X,false):-fail)),
    asserta((v(X,true):-fail)),
   (\+ground(X),
  asserta((v(not(X),true) :- ri(T,DeleteList))),!;asserta(v(not(X),true)))
 ;
  asserta(v(NT,false)), % otherwise
   (\+ground(NT),
  asserta((v(NT,true) :- ri(NT,DeleteList))),!;asserta(v(NT,true)))
             ),
  (C\=[],states(C, DeleteList),!;true).
```

The predicate `states` declares as true predicates of `S2` by transforming them into terms, which will become part of the dynamic predicate **`v`**.
Thus, `v (predicate of S2)` declares the predicate true, whether it is negated or not.
The predicate is not affirmed as `assert(predicate).`, this because it would not have easily been possible to cancel with a predicate like `retractall`.

However, **`v` can have two arguments**:

```prolog
v(predicate, {false,true}):
v(predicate,false) = v(predicate)
v(predicate,true) = v(predicate) + unification of any variables with the Goal launched

EXAMPLE:
S1=[plays(andrea,basket)]
S2=[plays(X,Y)]
v(S1,true)=>S2=[plays(andrea,basket)]
```

**`ri`** is the predicate that unifies and re-instantiates all the predicates.

```prolog
ri(T,DeleteList):-
    nb_getval(list,F), member(T,F),instanciate(F,DeleteList).
```

In this case, however, no complex logical consequences can be created. We remain in the case of goal regression with "Goals subset of .."

The predicate **`general_consequences/2`** needs to build more complex models.

Example:

```prolog
general_consequences(plays(andrea,basket), Unifica) :- v(tall(andrea,1.80), true)
```

**`general_consequences(predicate,Unify)`**=>**`Unify`** is used to be able to launch `v` with the possibility of unifying its variables or not.

Therefore, if

```prolog

S1=[plays(andrea,basket)]
S2=[tall(andrea,X)]

general_consequences(S1,S2,false) => true => X=1.80
```

Finally, in the case of planning, we may have general rules.

Example:

```prolog
plays(andrea,basket) :-  \+ v(plays(marco, basket))

S1=[plays(andrea,basket)]
S2=[plays(marco,basket), plays(andrea,basket)]

logical_consequence(S1,S2,false) => false
```

These general rules are declared in the program a priori.

For example for the block scheduling problem (file **generic_problem.pl**):

```prolog
%general constraints
above(X,Y) :- \+(v(free_item(Y))), \+(v(took(Y))), \+(v(took(X))).
free_item(X) :- \+ (v(above(_,X))), \+ (v(took(X))).
took(X) :- \+ (v(took(Y)), X\==Y), \+ (v(above(X,_))), \+ (v(above(_,X))),
  \+(v(freehand)).
```



The predicate **`infer(Goal, Contr) :-`** checks whether the Goal is a logical consequence of the new status declared.

`Contr` is a variable that takes the values `{false, true}`, if **true** symbolizes we are looking for a **contradiction.**

Contradictions are found when  **A and not(A) is true, or when the general rules applied are violated. **

Example:

```prolog
S1=[plays(andrea,basket)]
S2=[plays(marco,basket)]
#no contraint
\+ contradiction(S1,S2,false) => true

If we affirm instead
plays(andrea,basket) :- \+ v(plays(marco,basket))=>
\+ contradiction(S1,S2,false) => false
```



Finally, there are predicates such as

**`logical_consequence_list(S1, S2, DeleteList, Contr, ListLogicalConsequences, LitNotLogicalConsequences)`**


which are variants of `logical_consequence`, but are used in some cases, such as the calculation of the heuristic value.



## **DIFFERENCES WITH THE ORIGINAL PLANNER**

```prolog
test(Plan) :- 
 initialState(S),
 goal(G),
 sort(G, GO),
 findPlans2(S, [0-[GO,0,[],[],0]], 8, Plan, Cost),
 write("final cost "),
 writeln(Cost),
 write2(Plan).
```

The planner expands all possible nodes by proceeding via A*:

```prolog
% findPlans2 (+ State, + ActionSequence, + Plan)
findPlans2(State, [_-[GO,Cost,SeqGoals,Plan,_]|_], _, Plan, Cost):-
  logical_consequence(GO, State,false).

% findPlans2 (+ Status, + ActionSequence, + MaxDepth, -Plan)
% Depth Max starting from a certain node
% A represents the A* value of the current node
% planner A*
findPlans2(State, [A-P|Actions], MaxDepth, Plan, Cost)  :-
 selectPlans2(State,A-P, MaxDepth, ExtendedActions),
 mergeSort(ExtendedActions,Actions,NewActions),
 findPlans2(State, NewActions, MaxDepth, Plan, Cost).
```

`MaxDepth` is used to stop the search when it arrives, starting from node i, to expand more than `MaxDepth` nodes.

```prolog
% selectPlans(+ InitialState,+ ActionSequence,+ MaxDepth, -ExtendedActions)
selectPlans2(InitialState, _-[Goal, InitialCost,Seq,Plan,P], MaxDepth, ExtendedActions ):-
    findall(A - [GO,CC,[GO|Seq],[Action|Plan], PP], % from best node expands all possible new nodes
 (member(G,Goal),operator(Action,Prec,Del,Add), % such that there is an operator that,
    \+ useless(Action,Plan), % avoid performing unnecessary actions compared to the previous one
     logical_consequence([G],Add,false), %by performing the Action, it satisfies at least one of the goals
     PP is P + 1, % occurs we are not beyond the maximum depth allowed
     PP =< MaxDepth,%writeln(PP),
     subtract(Goal,[G], RemainingGoals),
     check_remains(RemainingGoals, Add, [], List), % check if other Goals are a logical consequence of the new state where we will be
     subtract(RemainingGoals, List, RegressGoals0),
     non_contradiction(RegressGoals0, Prec,false), % verifies that the unsatisfied Goals and the Operator's Preconditions do not create an unreachable state
     \+ contradiction(RegressGoals0,Del,true), % check that the unsatisfied Goals are not canceled from the goals to be achieved
     append(RegressGoals0, Prec, RegressGoals),
     sort(RegressGoals, GO),
     \+ examined(GO,Seq), % avoid loops in the graph => TO IMPROVE
     compute_heuristics(Prec, InitialState,false,false,Heuristics), % this is a general "heavy" heuristic, you can do better depending on the context
     compute_cost(Action,Cost),
     CC is Cost + InitialCost,
     A is CC + Heuristics
     ),List),
     sort(List, Sorted0),
     keysort(Sorted0, ExtendedActions).
```

`non_contradiction` works as a `contradiction`, but unifies any variables between the first and second lists.

`useless`  is a predicate that avoids performing unnecessary actions given the previous one.

Ex.
move_on(staircase), move_down(staircase) => may be useless

The predicate `examined` was reformulated as follows:

```prolog
examined(GO,[T|_]) :-
  ( ground(GO),
    logical_consequence(T,GO,false)),
  !.
examined(GO,[_|C]) :-
	examined(GO,C).
```

If `GO` contains a variable, `examined` could say that Goal has already been visited, when in fact it could represent a new state.

Therefore, in the case of the monkey problem, **does not avoid loops.**

**For the calculation of the heuristic, if there are not too many operators, it provides in a good time a general estimate of how many actions (with an estimated cost of 1) still need to be performed to reach the final goal.**

```prolog
% compute_heuristics(+S1,+S2,+DeleteList, +Contr, -Costo)
% h(X) = 0 if X = Goal
compute_heuristics(S1,S2,DeleteList, Contr, Cost):-
logical_consequence_list(S1,S2,DeleteList, Contr, _, NL),
setof(C, NL^(underestimate(NL, 0, C, [])), [Cost|_]),!.
```

It takes a first list of Goals that are not satisfied with the current `S2` state, then `underestimates` the number of actions that should be performed to reach the goal.

 

## BEST ACTION PLAN RESPECTING RESOURCE CONSTRAINT

In order to find the least cost path that respects a SINGLE resource constraint (e.g. 1000 kilocalories of energy), ALL the possible ACTION PLANS are found first, then the **model of the shortest path** is applied.

Since the graph has positive costs, the computation time is polynomial. However, in some cases, such as the monkey problem, it could be very onerous to calculate ALL possible action plans, if you can't do a good multipath pruning. Then, in the program it is possible to find a first list of routes with minimum cost, on which the model of the route with minimum cost is applied. The solution may therefore not be the optimal one, or it may even not be found even if one exists.

```prolog
testResearchOperative :- 
 initialState(S),
 goal(G),
 sort(G, GO),
 findall(Plan, findPlans(S, [0-[GO,0,[],[],0]], 8, Plan), Lists),
 solveOperatingResearch(Lists,100).
```

```prolog
% solveOperatingResearch (+ ActionPlistsList, + ResourceConstraint)
% from the possible lists of action plans found, calculates the solution at the lowest cost that respects the resource constraints
solveOperatingResearch(Lists, ResourceConstraint):-
findElements(0, Lists, [], X), %create the graph
describeNetwork(X, Aims,Resources, _, 0,_ ),
sum(Resources, #=<, ResourceConstraint),
sum(Aims, #= , U),
labeling([min(U)], Aims),
rebuildPlan(X,Plan),
write("final cost: "), writeln(U),
writeln("the plan is as follows ===> "),
write2(Plan).
```

With `findall` we can find the action plans, with `findElements` we build the direct graph, with `describeNetwork` we assign the variables to the arcs.

```prolog
% findElements (+ Position, + ListActionPlans, + Accumulator, - Graph)
% allows you to create the network, that is the oriented graph for the calculation of the minimum path
findElements(_, [], _, []):-!.
findElements(Pos, [List|Tail], Acc, X):-
 P is Pos + 1,
 (nth0(Pos, List, SearchedElement),!, % extracts the Pos element to search from the List (1,2, ..)
 findNextNode(Pos, [List|Tail], SearchedElement, [], Nodes), % returns all action plans .. sees if the same type of action is in the Pos position
 combinations(Nodes, [], Lists), % the lists are extracted with the action SearchedElement in common
 findElements(P, Lists, [SearchedElement|Acc], M1), % creates new nodes starting from the shared node SearchedElement
 differentLists(Nodes,[], DifferentLists), % searches for action plans that do not fit in the SearchedElement node
 findElements(Pos, DifferentLists, Acc, M2), % creates new nodes starting from the current node
 X = [SearchedElement + NN - M1, M2],!; % SearchedElement + Variable Not Instantiated - Adjacent Nodes | Alternatives
 findElements(Pos, Tail, Acc, X)).
```

`findElements` works in this way:
takes the first action plan, and checks what is the first action to be taken, which becomes one of the POSSIBLE STARTING NODES.
Check, through `combinations`, if there are other lists that contain that node as the starting one.
Check, through `differentLists`, if there are other lists that DO NOT contain that node as the starting one.
The shared lists become the source of a new search, which will lead to finding the list of all possible actions starting from the starting node.
The other lists become the origin of a new search, which will lead to finding the list of all the possible alternatives to the starting node.

Example: if the first list has A as its first action, and B as its second, if the second list has A as its first action, and C as its second action, findElements will return the following result:

`X = [A + NN - [B + NN - [] | [C + NN - [] | []]]`

`NN` represents the variable that indicates whether or not that NODE, that is an ACTION, will be chosen in the shortest path.
