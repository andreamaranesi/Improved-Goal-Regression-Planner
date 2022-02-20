
% given the initial list [HEAD | TAIL], returns the List of GOALS which are a logical consequence of the STATE which is
% true for Previous Goals, according to certain term values
check_remains([],_,Acc,Acc):-!.
check_remains([Goal|Tail], Add, Acc, List):-
  (logical_consequence([Goal], Add, false),check_remains(Tail,Add,[Goal|Acc],List),!;
  check_remains(Tail, Add, Acc, List)).


% find_contradictions (+ List1, + List2, + DeleteList, -ListC)
% DeleteList = {true, false} => if true the literals of List2 are negated
find_contradictions(List1,List2, DeleteList,ListC):-
    logical_consequence_list(List1,List2,DeleteList,true,_,ListC).


one_logical_consequence([],_):-!,fail.
one_logical_consequence([T|C],Contr):-
    ( see([T], Contr),!;one_logical_consequence(C,Contr)).

% one_logical_consequence (+ List1, + List2, + DeleteList, + Contradictions)
% DeleteList = {true, false} => if true the literals of List2 are negated
% Contradictions = {true, false} => if true means we are looking for contradictions
% true if at least one element of List1 is a logical consequence of List2
one_logical_consequence(S1,S2,DeleteList, Contr):-
  S1\=[],
  nb_linkval(list,S2),
  instanciate([S2], DeleteList),
  one_logical_consequence(S1, Contr).


% logical_consequence_list (+ List1, + List2, + DeleteList, + Contradictions, -LogicalConsequences, -NonLogicalConsequences)
% DeleteList = {true, false} => if true the literals of List2 are negated
% Contradictions = {true, false} => if true means we are looking for contradictions
logical_consequence_list([],_,_, _,[], []):-!.
logical_consequence_list(S1,S2,DeleteList, Contr,LogicalConsequences, NonLogicalConsequences):-
  nb_linkval(list,S2),
  logical_consequences_list(S1,S2,DeleteList, Contr, LogicalConsequences),
  subtract(S1,LogicalConsequences,NonLogicalConsequences).

% recursiveNot (+ Predicate, + ReducedPredicate)
% transform a predicate like not(not(not(not(X)))) in X
recursiveNot(T,NT):-
 T==not(not(X)), !, recursiveNot(X, NT);
 NT=T.


% states (+ List, + DeleteList)
% DeleteList = {true, false} => if true the literals of List2 are negated
% Contradictions = {true, false} => if true means we are looking for contradictions
% instantiates the conditions in List as true in their current state
states([],_):-!.
states([[T|C]|Tail],DeleteList):-!,
  states([T|C],DeleteList), states(Tail, DeleteList).

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
  asserta(v(NT,false)), % altrimenti
   (\+ground(NT),
  asserta((v(NT,true) :- ri(NT,DeleteList))),!;asserta(v(NT,true)))
             ),
  (C\=[],states(C, DeleteList),!;true).

% logical_consequence (+ List1, + List2, + DeleteList)
% DeleteList = {true, false} => if true the literals of List2 are negated
% true only if it is a logical consequence
logical_consequence(S1,S2, DeleteList) :-
  nb_linkval(list, S2 ), % we need nb_linkval to keep a pointer to the list that contains new clauses
  instanciate([S2],DeleteList),
  see(S1, false).

% logical_consequences_list (+ List1, + List2, + DeleteList, + Contradictions, -ListLogicalConsequences)
% DeleteList = {true, false} => if true the literals of List2 are negated
% Contradictions = {true, false} => if true means we are looking for contradictions
% initializes the variables of List1 and those of the Current State
logical_consequences_list([T|C],S2, DeleteList, Contr, List):-
  instanciate([S2],DeleteList),
  logical_consequences_list([T|C],Contr, [], List).


logical_consequences_list([],_,Acc,Acc):-!.
logical_consequences_list([T|C],Contr,Acc,List):-
  (\+(\+see([T], Contr)), logical_consequences_list(C,Contr,[T|Acc],List), !); % \+(\+) is used to not instantiate any variable of the current state
  (logical_consequences_list(C,Contr,Acc,List)).

% contradiction (+ List1, + List2, + DeleteList)
% DeleteList = {true, false} => if true the literals of List2 are negated
% If an element of List1 contradicts List2 (the current State), returns true
contradiction(_,[],_):- !,fail.
contradiction([],_, _):-!,fail.
contradiction(S1,S2, DeleteList):-
   nb_linkval(list, S2 ),
  (instanciate([S2], DeleteList),
  \+ see(S1, true)),!. % \+ allows to not instantiate any variable of the current state


% non_contradiction (+ List1, + List2, + DeleteList)
% Same as contradiction predicate, but possibly instantiates variables of List2 (the current State)
non_contradiction(_,[],_).
non_contradiction([],_, _).
non_contradiction(S1,S2, DeleteList):-
   nb_linkval(list, S2 ),
  (instanciate([S2], DeleteList),
  see(S1, true)).


% is_list_logical_consequence (+ List1, + List2, + DeleteList)
% does not allow backtracking
% true if S1 is a logical consequence of S2
is_list_logical_consequence(S1,S2,DeleteList):-
 logical_consequence_list(S1,S2, DeleteList, false, List,_),
 List\=[],!. % red cut

% allows to reinitialize the variables of the current state, possibly modified in runtime
reinstanciate :-
       v(deleteList,DeleteList),
                   nb_getval(list,F), instanciate(F,DeleteList).

% instanciate(+State, +DeleteList)
% declares as true the state list
instanciate([T|C], DeleteList):-
  abolish(v/1),
  abolish(v/2),
  asserta(v(deleteList,DeleteList)),
  states([T|C], DeleteList),
  (C\=[],states(C, DeleteList),!;true).

% see(+ Goals, + Contradiction)
% true only if all Goals are a logical consequence of the current state (initialized as "metastate")
see([],Contr):- Contr=true.
see([G1|G2],Contr):-
  infer(G1, Contr),(G2\=[],see(G2, Contr);G2=[]).


% infer (+ Goals, + Contradiction)
% true only if the Current Goal is a logical consequence of the current state (initialized as "metastate")
% v(Goal) => predicate which states whether Goal is true or not in the initialized state
% Goal :- Body => Horn clause on the Goal in the original program. Represents a constraint / rule.
% general_consequences(Goal) => predicate that allows you to see if the Goal is true, in the current state, by logical consequence
infer(Goal, Contr):-
  findall([K,Goal], clause(v(Goal,true),K),Ks1), % find predicates v(Goal)
  findall([K,Goal], clause(general_consequences(Goal,true),K),GeneralConsequences1), % find predicates general_consequences(Goal)
  append(GeneralConsequences1,Ks1,Ks4),
   (Ks4\=[],!, % if there is at least one predicate to analyze for the current goal
    ( Ks1\=[], !,sort(Ks1,Ks22), launch(Goal, Ks22); launch(Goal, GeneralConsequences1);
                                     Contr=true,
                                  reinstanciate) % reinitialize because it launch(Goal, GeneralConsequences1) may have instantiated some variables of the current state list
    ;Contr=true), % if there is not at least one predicate to analyze .. the Goal is not a logical consequence, but it does not mean it is a contradiction
(Goal=not(Y),!,findall([K,Y], clause(Y,K),Ks5), % if the goal is a negated predicate
   ( Ks5=[];\+launch(Goal,Ks5)); % must not contradict the constraints established for it
   findall([K,Goal], clause(Goal,K),Ks5), % otherwise it must respect the established constraints / rules
   launch(Goal, Ks5)).

% ri(+Goal, +DeleteList)
% if the Goal is part of one of the members of the initialized list, then the current state, or the Goal, could be assigned to the variables
ri(T,DeleteList):-
    nb_getval(list,F), member(T,F),instanciate(F,DeleteList).


n_subset([H|Tail], List):-
    member(H, List),
    n_subset(Tail, List).
n_subset([], _).

% launch(+Goal, +ListPredicatesToCall)
% launches predicates in the list
launch(_, []):-!.
launch(Goal,[[T|[UnifiedGoal]]|Tail]):-
   (launch(T),Goal=UnifiedGoal; % if the predicate is true, the current Goal eventually unifies with the head of the clause
   Tail\=[],   reinstanciate, % if the predicate fails and the queue is not empty, re-instate the current state, and retry with the next clause
    launch(Goal,Tail)).

launch(true):-!.
launch(Goals):-call(Goals).
