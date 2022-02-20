operatore(si_muove_da_a(X,Y),                                % si_muove_da_a(X,Y)
    [inp(scimmia,X), sopra(scimmia,pavimento)],
    [inp(scimmia,X)],
    [inp(scimmia,Y)]).
operatore(push(B,X,Y),                                             % push(B,X,Y)
    [inp(scimmia,X), inp(B,X), sopra(scimmia,pavimento), sopra(B,pavimento)
    ],
    [inp(scimmia,X), inp(B,X)],
    [inp(scimmia,Y), inp(B,Y)]):-B=scala.
operatore(sale_su(B),                                                % sale_su(B)
    [inp(scimmia,X), inp(B,X), sopra(scimmia,pavimento), sopra(B,pavimento)],
    [sopra(scimmia,pavimento)],
    [sopra(scimmia,B)]):-B=scala.
operatore(prende(B),                                                  % prende(B)
    [sopra(scimmia,scala), inp(scala,X), inp(B,X), stato(B,pendenti)],
    [stato(B,pendenti)],
    [stato(B,prese)]):-B=banane;B=altro.
operatore(scende_da(B),                                             % scenda da(B)
    [inp(scimmia,X), inp(B,X), sopra(scimmia,B), sopra(B,pavimento)],
    [sopra(scimmia,B)],
    [sopra(scimmia,pavimento)]).
operatore(appendi(B,X),                                            % appende B in X
    [stato(B,prese), sopra(scimmia,scala),inp(scala,X), sopra(scala,pavimento)],
    [stato(B,prese)],
    [stato(B,pendenti), inp(B,X)]):-B=banane;B=altro.
% operatore(applaudi,                                            % applaudi
 %   [stato(B,pendenti), sopra(scimmia,pavimento)],
 %  [stato(B,pendenti)],
 %   [stato(B,prese)]).


% costi degli operatori
costo(applaudi(_),1).
costo(_,1).

% azioni inutili se subito prima ne � stata compiuta un'altra specifica
inutile(_,[]):-!,fail.
inutile(Azione,[T|_]):-
Azione=sale_su(X), T=scende_da(X),!;
Azione=scende_da(X), T=sale_su(X),!;
Azione=push(X,_,_), T=push(X,_,_).

% regole sugli operatori
inp(X,Z) :- \+ (v(inp(X,Y)), Y\=Z).
sopra(X,Y) :- \+ (v(sopra(X,Z)), Y\=Z).


statoIniziale([sopra(scimmia,pavimento),
               sopra(scala,pavimento),
               inp(scimmia,g),
               inp(scala,soffitta),
               stato(banane,pendenti),
               inp(altro,cantina),
               inp(banane,cantina)
               ]).

goal([inp(banane,j)]).

cons_generiche(stato(B,prese),Unifica) :- v(premio(B), Unifica).
