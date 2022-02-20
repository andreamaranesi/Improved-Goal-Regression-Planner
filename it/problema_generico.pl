operatore(lascia(X),
    [preso(X)],
    [preso(X)],
    [su_tavolo(X), libero(X), manolibera]).

operatore(poggia(X,Y),
    [preso(X), libero(Y)],
    [preso(X), libero(Y)],
    [manolibera, sopra(X,Y), libero(X)]).

operatore(togli(X,Y),
    [sopra(X,Y), manolibera, libero(X)],
    [sopra(X,Y), manolibera, libero(X)],
    [preso(X), libero(Y)]).

operatore(prendi(X),
    [su_tavolo(X), libero(X), manolibera],
    [su_tavolo(X), libero(X), manolibera],
    [preso(X)]).

% costi operatori
costo(_,1).

%vincoli sugli operatori
sopra(X,Y) :- \+(v(libero(Y))), \+(v(preso(Y))), \+(v(preso(X))).
libero(X) :- \+ (v(sopra(_,X))), \+ (v(preso(X))).
preso(X) :- \+ (v(preso(Y)), X\==Y), \+ (v(sopra(X,_))), \+ (v(sopra(_,X))),
  \+(v(manolibera)).


statoIniziale([libero(b),
	       libero(c),
	       preso(a),
	       su_tavolo(c),
	       su_tavolo(b)]).

goal([sopra(a,b),
      sopra(b,c)]).

