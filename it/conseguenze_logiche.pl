% data la lista iniziale [TESTA|CODA], ritorna la Lista di GOAL che sono conseguenza logica dello STATO che è
% vero per i Goal Precedenti, secondo certi valori dei termini
verifica_rimanenti([],_,Acc,Acc):-!.
verifica_rimanenti([Goal|Coda], Add, Acc, Lista):-
  (conseg_logica([Goal], Add, false),verifica_rimanenti(Coda,Add,[Goal|Acc],Lista),!;
  verifica_rimanenti(Coda, Add, Acc, Lista)).



% trova_contraddizioni(+Lista1,+Lista2,+DeleteList,-ListaC)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
trova_contraddizioni(Lista1,Lista2, DeleteList,ListaC):-
    lista_conseg_logica(Lista1,Lista2,DeleteList,true,_,ListaC).


una_conseg_logica([],_):-!,fail.
una_conseg_logica([T|C],Contr):-
    ( vedi([T], Contr),!;una_conseg_logica(C,Contr)).

% una_conseg_logica(+Lista1,+Lista2,+DeleteList, +Contraddizioni)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
% Contraddizioni={true,false} => se true significa stiamo cercando contraddizioni
% true se almeno un elemento di Lista1 è conseguenza logica di Lista2
una_conseg_logica(S1,S2,DeleteList, Contr):-
  S1\=[],
  nb_linkval(lista,S2),
  inizializza([S2], DeleteList),
  una_conseg_logica(S1, Contr).


% lista_conseg_logica(+Lista1,+Lista2,+DeleteList, +Contraddizioni, -ConseguenzeLogiche, -NonConseguenzeLogiche)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
% Contraddizioni={true,false} => se true significa stiamo cercando contraddizioni
lista_conseg_logica([],_,_, _,[], []):-!.
lista_conseg_logica(S1,S2,DeleteList, Contr,ConseguenzeLogiche, NonConseguenzeLogiche):-
  nb_linkval(lista,S2),
  lista_conseg_logiche(S1,S2,DeleteList, Contr, ConseguenzeLogiche),
  subtract(S1,ConseguenzeLogiche,NonConseguenzeLogiche).

% notRicorsivi(+Predicato,-PredicatoSenzaNotRicorsivi)
notRicorsivi(T,NT):-
  T==not(not(X)), !, notRicorsivi(X,NT);
  NT=T.
 

% afferma(+Lista,+DeleteList)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
% Contraddizioni={true,false} => se true significa stiamo cercando contraddizioni
% istanzia come vere nello stato attuale le condizioni in Lista
afferma([],_):-!.
afferma([[T|C]|Coda],DeleteList):-!,
  afferma([T|C],DeleteList), afferma(Coda, DeleteList).
afferma([T|C], DeleteList):-
  notRicorsivi(T, NT), % se il predicato è del tipo not(not(not(not(X)))), NT diventa X
  asserta((v(NT) :- v(NT,false))),
  (
      (NT=not(X),!;DeleteList=true,X=NT),!, % se dobbiamo negare l'affermazione
   asserta((v(X):- v(X,false))),
   asserta(v(not(X),false)),   % diciamo che v(not(Affermazione)) è vera
    asserta((v(X,false):-fail)),
    asserta((v(X,true):-fail)),
   (\+ground(X),
  asserta((v(not(X),true) :- ri(T,DeleteList))),!;asserta(v(not(X),true)))
 ;
  asserta(v(NT,false)), % altrimenti
   (\+ground(NT),
  asserta((v(NT,true) :- ri(NT,DeleteList))),!;asserta(v(NT,true)))
             ),
  (C\=[],afferma(C, DeleteList),!;true).



%conseg_logica(+Lista1,+Lista2, +DeleteList)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
%true solo se è possibile affermare è una conseguenza logica
conseg_logica(S1,S2, DeleteList) :-
  nb_linkval(lista, S2 ), % nb_linkval ci serve per manetenere un puntatore a S2
  inizializza([S2],DeleteList),
  vedi(S1, false).

%conseg_logica(+Lista1,+Lista2, +DeleteList, +Contraddizioni, -ListaConseguenzeLogiche)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
% Contraddizioni={true,false} => se true significa stiamo cercando contraddizioni
% inizializza le variabili di Lista1 e quelle dello Stato Attuale
lista_conseg_logiche([T|C],S2, DeleteList, Contr, Lista):-
  inizializza([S2],DeleteList),
  lista_conseg_logiche([T|C],Contr, [], Lista).


lista_conseg_logiche([],_,Acc,Acc):-!.
lista_conseg_logiche([T|C],Contr,Acc,Lista):-
  (\+(\+vedi([T], Contr)), lista_conseg_logiche(C,Contr,[T|Acc],Lista), !); % \+(\+) serve per non istanziare nessuna variabile dello stato attuale
  (lista_conseg_logiche(C,Contr,Acc,Lista)).

%contraddizione(+Lista1,+Lista2, +DeleteList)
% DeleteList={true,false} => se true i letterali di Lista2 vengono negati
% Se un elemento di Lista1 è in contraddizione con la Lista2 (lo Stato corrente), restituisce true
contraddizione(_,[],_):- !,fail.
contraddizione([],_, _):-!,fail.
contraddizione(S1,S2, DeleteList):-
   nb_linkval(lista, S2 ),
  (inizializza([S2], DeleteList),
  \+ vedi(S1, true)),!. % il \+ vedi permette di non istanziare nessuna variabile dello stato attuale


%non_contraddizione(+Lista1,+Lista2, +DeleteList)
% Uguale al predicato contraddizione, ma istanzia eventualmente variabili di Lista2 (lo Stato corrente)
non_contraddizione(_,[],_).
non_contraddizione([],_, _).
non_contraddizione(S1,S2, DeleteList):-
   nb_linkval(lista, S2 ),
  (inizializza([S2], DeleteList),
  vedi(S1, true)).


%uno_conseg_logica(+Lista1,+Lista2, +DeleteList)
% non consente il backtracking
% true se S1 è conseguenza logica di S2
uno_conseg_logica(S1,S2,DeleteList):-
 lista_conseg_logica(S1,S2, DeleteList, false, Lista,_),
 Lista\=[],!. % red cut

% consente di reinizializzare le variabili dello stato attuale, eventualmente modificate in runtime
reinizializza :-
       v(deleteList,DeleteList),
                   nb_getval(lista,F), inizializza(F,DeleteList).

% inizializza(+Stato, +DeleteList)
% dichiara come vera la lista Stato
inizializza([T|C], DeleteList):-
  abolish(v/1),
  abolish(v/2),
  asserta(v(deleteList,DeleteList)),
  afferma([T|C], DeleteList),
  (C\=[],afferma(C, DeleteList),!;true).

% vedi(+Goals, +Contraddizione)
% true solo se tutti i Goals sono conseguenza logica dello stato attuale (inizializzato come "metastato")
vedi([],Contr):- Contr=true.
vedi([G1|G2],Contr):-
  deduci(G1, Contr),(G2\=[],vedi(G2, Contr);G2=[]).


% deduci(+Goals, +Contraddizione)
% true solo se il Goal Corrente è conseguenza logica dello stato attuale (inizializzato come "metastato")
% v(Goal)=> predicato che afferma se il Goal è vero o meno nello stato inizializzato
% Goal :- Corpo => clausola di Horn sul Goal nel programma originario. Rappresenta un vincolo/regola.
% cons_generiche(Goal) => predicato che consente di vedere se il Goal è vero, nello stato attuale, per conseguenza logica
deduci(Goal, Contr):-
  findall([K,Goal], clause(v(Goal,true),K),Ks1), % trova i predicati v(Goal)
  findall([K,Goal], clause(cons_generiche(Goal,true),K),ConsGeneriche1), % trova i predicati cons_generiche(Goal)
  append(ConsGeneriche1,Ks1,Ks4),
   (Ks4\=[],!, % se è presente almeno un predicato da analizzare per il goal corrente
    ( Ks1\=[], !,sort(Ks1,Ks22), lancia(Goal, Ks22); lancia(Goal, ConsGeneriche1);
                                     Contr=true,
                                  reinizializza) %reinizializziamo perché lancia(Goal, ConsGeneriche1) potrebbe aver instanziato alcune variabili dello stato corrente
    ;Contr=true), % se non è presente almeno un predicato da analizzare.. il Goal non è conseguenza logica, ma non significa sia una contraddizione
(Goal=not(Y),!,findall([K,Y], clause(Y,K),Ks5), % se il goal è un predicato negato
   ( Ks5=[];\+lancia(Goal,Ks5)); % non deve essere in contraddizione con i vincoli per lui stabiliti
   findall([K,Goal], clause(Goal,K),Ks5), % altrimenti deve rispettare i vincoli/regole stabilite
   lancia(Goal, Ks5)).

% ri(+Goal, +DeleteList)
% se il Goal fa parte di uno dei membri della lista inizializzata, allora lo stato attuale, o il Goal, potrebbe subire un assegnamento alle variabili
ri(T,DeleteList):-
    nb_getval(lista,F), member(T,F),inizializza(F,DeleteList).


sottoinsieme([T|Coda], Lista):-
    member(T, Lista),
    sottoinsieme(Coda, Lista).
sottoinsieme([], _).

% lancia(+Goal, +ListaPredicatiDaChiamare)
% lancia i predicati nella lista
lancia(_, []):-!.
lancia(Goal,[[T|[GoalUnificato]]|Coda]):-
   (lancia(T),Goal=GoalUnificato; % se il predicato è true, il Goal attuale unifica eventualmente con la testa della clausola
   Coda\=[],   reinizializza, % se il predicato fallisce e la coda non è vuota, si reinstanzia lo stato corrente, e si riprova con la clausola successiva
    lancia(Goal,Coda)).

lancia(true):-!.
lancia(Goals):-call(Goals).
