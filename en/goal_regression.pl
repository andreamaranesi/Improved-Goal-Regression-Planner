
:- use_module(library(clpfd)).

:-dynamic(v/2).
:-dynamic(v/1).
:-dynamic(general_consequences/2).

:-dynamic(operator/4).
:-dynamic(cost/2).
:-dynamic(useless/2).

:- [logical_consequences].
:- [second_problem].


% findNextNode (+ Position, + ListOfActionPlans, + SearchedElement, + Accumulator, -ListOfActionPlans)
% returns the list of action plans which up to the Pos position are equal to each other
% creates common nodes in the direct graph
 findNextNode(_,[],_,Acc,Acc):-!.
findNextNode(Pos, [List|Tail], SearchedElement, Acc, Nodes):-
  P is Pos + 1,
 nth0(Pos, List, Element),
 (Element==SearchedElement),
 (nth0(P, List, Successor), ground(Successor),!,
 findNextNode(Pos, Tail, SearchedElement,[Successor - List|Acc],Nodes),!; % the queue is visited and the Successor for each List is accumulated
 findNextNode(Pos,Tail,SearchedElement, [null1 - List | Acc],Nodes)); % if the next item in the Current List is the last one
 findNextNode(Pos, Tail, SearchedElement,[null0 - List|Acc],Nodes). % if Element \== SearchedElement

% combinations (+ ListOfCommonActionPlans, + Accumulator, -NodesInCommon)
% returns all common lists up to a certain action, which do not have a variable or the term of the action plan as their successor
combinations([], Acc, Acc):-!.
combinations([Successor - List|Tail], Acc, Lists):-
(Successor \== null1,
 Successor \== null0,!,
 combinations(Tail, [ List | Acc], Lists),!; combinations(Tail,Acc,Lists)).

% differentLists (+ List of Common Action Plans, + Accumulator, -Lists)
% returns all lists that do not fit in a certain node
differentLists([], Acc, Acc):-!.
differentLists([Successor - List|Tail], Acc, Lists):-
(Successor==null0,!,
 differentLists(Tail, [ List| Acc], Lists); differentLists(Tail,Acc,Lists)).

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

% findAlternativeNodes(+CurrentNode, +Accumulator, -Nodes)
% finds Nodes that are part of alternative paths
findAlternativeNodes([], Acc, Acc):-!.
findAlternativeNodes([El + C1 + C2 - M1,M2|_],Acc,X):-
 findAlternativeNodes(M2, [El|Acc],X).

% findNodesSuccessors (+CurrentNode, +Accumulator, -NodesSuccessors)
findNodesSuccessors([], Acc, Acc):-!.
findNodesSuccessors([El + C1 + C2 + J - M1,M2|_],Acc,X):-
 findNodesSuccessors(M1, [El|Acc],X).

% associates a variable Z >= 0 to each element
updateList([],Acc,Acc):-!.
updateList([T|C],Acc, List):-
 Z #>= 0,
 updateList(C, [Z|Acc],List).

% describeNetwork (+ Network, -Targets, -Resources, -Successors, -FullList, + Predecessor, + Var)
% SET CONSTRAINTS ON THE ARCHES OF THE NETWORK
% describes the network
% DestinationVariables are the end nodes of the graph
% Targets => variables that represents the total cost of the node, if selected, 0 otherwise
% Resourcelist => variables {0,1} * ResourceValue, represents the resource consumption if the node is selected
% Var represents the queue node.. ex. [a => {b,c,d}], from node a we can go to {b,c,d}, Var = a, if a is selected, one of {b,c,d} must be selected as well
describeNetwork([],[],[],[],_,_):-!.
%, writeln("no one").
describeNetwork([El + C1 + C2 + J - M1,M2|_], Targets, Resources, DestinationVariables, Predecessor, Var):-
   findAlternativeNodes(M2, [],Y),
   append([El], Y, Y1),
    (Predecessor=Al - List,Alternative is Al + 1, NewList=List,!;
    Alternative=1, updateList(Y1,[], NewList)), %Alternative=1 means they are arrival nodes from a common node, eg. from the source node
   (Predecessor=Al - List, !;
   Predecessor=0, sum(NewList, #=, 1) , !; % constraint source node
    K #= Var * (-1), append(NewList,[K],NewList0), sum(NewList0, #=, 0)), % constraint transit nodes, Var represents the queue node
   %write(El),write(" has as successors "),
   (Predecessor=Al - List, nth0(Al, List, Succ) ,!; nth0(0, NewList, Succ)),
   J = Succ, % J = 1 if the current node will be selected in the path
   Z #= C1 * Succ, % path constraint
   R #= C2 * Succ, % resource constraint
        (M1=[], Succ1=[J],Z1=[],R1=[],
      %  writeln("no one"),
        !; % destination goal
   describeNetwork(M1, Z1, R1, Succ1, 1, Succ)),
 %  write(El),write(" has as alternatives "),
   describeNetwork(M2, Z2, R2, Succ2, Alternative - NewList, _),
   append(Succ1,Succ2,DestinationVariables),
   (Predecessor=0, sum(DestinationVariables, #=, 1),!;true), % constraint destination goal
   append(Z1,Z2,Z3),
   append([Z],Z3,Targets),
    append(R1,R2,R3),
   append([R],R3,Resources),!.

% rebuildPlan (+ Network, -ActionOrderPlan)
% returns the ordered plan of actions selected in the graph
rebuildPlan([], []):-!.
rebuildPlan([El + C1 + C2 + J - M1,M2|_], Actions):-
  (J=1, Plan0=[El],!; Plan0=[]),
  rebuildPlan(M1, Plan1),
  rebuildPlan(M2, Plan2),
  append(Plan0, Plan1, Plan3),
  append(Plan3, Plan2, Actions).

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


% for simplicity, suppose each action consumes 1 resource
compute_resource(_,1).

%% finds the sequence of the minimum cost path, which respect the resource constraint
testResearchOperative :- % ==================================================================
 initialState(S),
 goal(G),
 sort(G, GO),
 findall(Plan, findPlans(S, [0-[GO,0,[],[],0]], 8, Plan), Lists),
 solveOperatingResearch(Lists,100).

%% CHECK THAT WE HAVE NOT ALREADY VISITED OTHER GOALS
examined(GO,[T|_]) :-
  ( ground(GO),
    logical_consequence(T,GO,false)),
  !.
examined(GO,[_|C]) :-
	examined(GO,C).


% compute_heuristics(+S1,+S2,+DeleteList, +Contr, -Costo)
% h(X) = 0 if X = Goal
compute_heuristics(S1,S2,DeleteList, Contr, Cost):-
logical_consequence_list(S1,S2,DeleteList, Contr, _, NL),
setof(C, NL^(underestimate(NL, 0, C, [])), [Cost|_]),!.


% underestimate(+List, +Sum, -Cost, +AddList)
underestimate([],Sum,Sum,_):-!.
underestimate(List,Sum, Cost, AddList):-
 NewSum is Sum + 1,
 operator(_,_,_,D),
 logical_consequence_list(List, D, false, false, _, NCL), % gets a first list of unsatisfied goals from the selected operator
 \+(verified(D, AddList)), % needs to avoid inspecting the same operator again
 (NCL=List, !, underestimate(List, NewSum, Cost,[D|AddList]); % if the whole list is not a logical consequence, try another operator
 underestimate(NCL, NewSum, Cost, [D])). % otherwise re-launch the predicate with the new non-logical consequences

% checks that the first list is not a subset of the second
verified([],_) :- !,false.
verified(_,[]) :- !,false.
verified(List,[Head|_]):-
 List=Head,!.
verified(List,[_|Tail]):-
 verified(List,Tail).

compute_cost(Action, Cost):-
 cost(Action,Cost),
 !.

% findPlans (+State, +ActionSequence, + Plan)
findPlans(State, [_-[GO,Cost,SeqGoals,Plan,_]|_], _, Plan):-
  logical_consequence(GO, State,false).

% findPlans (+ Status, + ActionSequence, + MaxDepth, -Plan)
% Depth Max starting from a certain node
% A represents the A* value of the current node
% planner A*
findPlans(State, [A-P|Actions], MaxDepth, Plan)  :-
 selectPlans(State,A-P, MaxDepth, ExtendedActions),
 mergeSort(ExtendedActions,Actions,NewActions),
 findPlans(State, NewActions, MaxDepth, Plan).

% selectPlans(+InitialState,+ActionSequence,+MaxDepth, -ExtendedActions)
selectPlans(InitialState, _-[Goal, InitialCost,Seq,Plan,P], MaxDepth, ExtendedActions ):-
    findall(A - [GO,CC,[GO|Seq],[Action + Cost + Resources|Plan], PP], % from best node expands all possible new nodes
 (member(G,Goal),operator(Action,Prec,Del,Add), % such that there is an operator that,
    \+ useless(Action,Plan), % avoid performing unnecessary actions compared to the previous one
     logical_consequence([G],Add,false), %by performing the Action, it satisfies at least one of the goals
     PP is P + 1, % occurs we are not beyond the maximum depth allowed
     PP =< MaxDepth,%writeln(PP),
     writeln(Action|Plan),
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
     A is CC + Heuristics,
     compute_resource(Action, Resources)
     ),List),
     sort(List, Sorted0),
     keysort(Sorted0, ExtendedActions).




%% SORT THE ACTIONS BY MINIMUM A* VALUE
mergeSort([],L,L).
mergeSort(L,[],L).
mergeSort([A1-P1|CP1],[A2-P2|CP2],[A1-P1|Coda]) :-	A1 =< A2,
	mergeSort(CP1,[A2-P2|CP2],Coda).
mergeSort([A1-P1|CP1],[A2-P2|CP2],[A2-P2|Coda]) :-
	A1 > A2,
	mergeSort([A1-P1|CP1],CP2,Coda).


%% find the path with minimum cost
test(Plan) :- % ==================================================================
 initialState(S),
 goal(G),
 sort(G, GO),
 findPlans2(S, [0-[GO,0,[],[],0]], 8, Plan, Cost),
  write("final cost "),
  writeln(Cost),
 write2(Plan).


% findPlans2 (+State, +ActionSequence, + Plan)
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



% selectPlans(+InitialState,+ActionSequence,+MaxDepth, -ExtendedActions)
selectPlans2(InitialState, _-[Goal, InitialCost,Seq,Plan,P], MaxDepth, ExtendedActions ):-
    findall(A - [GO,CC,[GO|Seq],[Action|Plan], PP], % from best node expands all possible new nodes
 (member(G,Goal),operator(Action,Prec,Del,Add), % such that there is an operator that,
    \+ useless(Action,Plan), % avoid performing unnecessary actions compared to the previous one
     logical_consequence([G],Add,false), %by performing the Action, it satisfies at least one of the goals
     PP is P + 1, % occurs we are not beyond the maximum depth allowed
     PP =< MaxDepth,%writeln(PP),
     writeln(Action|Plan),
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

:- set_prolog_flag(answer_write_options,[max_depth(0)]).


write2([T|C]) :- write(T),nl,
	write2(C).
write2([]).

