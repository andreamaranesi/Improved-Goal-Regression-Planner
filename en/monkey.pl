operator(move_from_to(X,Y),                                
    [inp(monkey,X), on(monkey,floor)],
    [inp(monkey,X)],
    [inp(monkey,Y)]).
operator(push(B,X,Y),                                            
    [inp(monkey,X), inp(B,X), on(monkey,floor), on(B,floor)
    ],
    [inp(monkey,X), inp(B,X)],
    [inp(monkey,Y), inp(B,Y)]):-B=staircase.
operator(go_up_on(B),                                                
    [inp(monkey,X), inp(B,X), on(monkey,floor), on(B,floor)],
    [on(monkey,floor)],
    [on(monkey,B)]):-B=staircase.
operator(take(B),                                                
    [on(monkey,staircase), inp(staircase,X), inp(B,X), status(B,hanging)],
    [status(B,hanging)],
    [status(B,took)]):-B=bananas;B=other.
operator(go_down_from(B),                                           
    [inp(monkey,X), inp(B,X), on(monkey,B), on(B,floor)],
    [on(monkey,B)],
    [on(monkey,floor)]).
operator(hang(B,X),                                         
    [status(B,took), on(monkey,staircase),inp(staircase,X), on(staircase,floor)],
    [status(B,took)],
    [status(B,hanging), inp(B,X)]):-B=bananas;B=other.
% operator(clap(B),                                         
 %   [status(B,hanging), on(monkey,floor)],
 %  [status(B,hanging)],
 %   [reward(B)]).

% costs of actions
cost(_,1).

% avoid useless actions
useless(_,[]):-!,fail.
useless(Action,[T|_]):-
Action=go_up_on(X), T=go_down_from(X),!;
Action=go_down_from(X), T=go_up_on(X),!;
Action=push(X,_,_), T=push(X,_,_).

% general rules
inp(X,Z) :- \+ (v(inp(X,Y)), Y\=Z).
on(X,Y) :- \+ (v(on(X,Z)), Y\=Z).


initialState([on(monkey,floor),
               on(staircase,floor),
               inp(monkey,g),
               inp(staircase,soffitta),
               status(bananas,hanging),
               inp(other,cantina),
               inp(bananas,cantina)
               ]).

goal([inp(bananas,j)]).

%general_consequences(status(B,took),Unify) :- v(reward(B), Unify).
