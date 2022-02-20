operator(leave(X),
    [took(X)],
    [took(X)],
    [on_table(X), free_item(X), freehand]).

operator(put_on(X,Y),
    [took(X), free_item(Y)],
    [took(X), free_item(Y)],
    [freehand, above(X,Y), free_item(X)]).

operator(take_off(X,Y),
    [above(X,Y), freehand, free_item(X)],
    [above(X,Y), freehand, free_item(X)],
    [took(X), free_item(Y)]).

operator(take(X),
    [on_table(X), free_item(X), freehand],
    [on_table(X), free_item(X), freehand],
    [took(X)]).

% operator costs
cost(_,1).


%general constraints
above(X,Y) :- \+(v(free_item(Y))), \+(v(took(Y))), \+(v(took(X))).
free_item(X) :- \+ (v(above(_,X))), \+ (v(took(X))).
took(X) :- \+ (v(took(Y)), X\==Y), \+ (v(above(X,_))), \+ (v(above(_,X))),
  \+(v(freehand)).


initialState([free_item(b),
	       free_item(c),
	       took(a),
	       on_table(c),
	       on_table(b)]).

goal([above(a,b),
      above(b,c)]).

