
:- use_module(library(clpfd)).

:-dynamic(v/2).
:-dynamic(v/1).
:-dynamic(cons_generiche/2).

:-dynamic(operatore/4).
:-dynamic(costo/2).
:-dynamic(inutile/2).

:-[conseguenze_logiche].
:-[scimmia].

setProsegui(Prosegui,Y):-
    (var(Prosegui),Prosegui=Y,!;true).

trovaElementi(Pos, Liste, X):-
 trovaElementi(Pos,Liste, [],X,_).

verificaPos(P,_,Max,_):-!.
verificaPos(P, Lista, Max, ElementiPrima):-
 nth0(P, ElementiPrima, Elemento),
 nth0(P,Lista, Elemento),
 PP is P+1,
 verificaPos(PP, El, Max, ElementiPrima).


% trovaNodoSucc(+Posizione, +ListaDiPianiDiAzione,+ElementoDaCercare, +Accumulatore, -ListaDiPianidiAzioneComuni)
% ritorna la lista dei piani d'azione che fino alla posizione Pos sono uguali tra di loro
% crea nodi comuni nel grafo diretto
 trovaNodoSucc(_,[],_,Acc,Acc):-!.
trovaNodoSucc(Pos, [Lista|Coda], ElementoDaCercare, Acc, Nodi):-
  P is Pos + 1,
 nth0(Pos, Lista, Elemento),
 (Elemento==ElementoDaCercare),
 (nth0(P, Lista, Successore), ground(Successore),!,
 trovaNodoSucc(Pos, Coda, ElementoDaCercare,[Successore - Lista|Acc],Nodi),!; % si visita la coda e viene accumulato il Successore per ogni Lista
 trovaNodoSucc(Pos,Coda,ElementoDaCercare, [null1 - Lista | Acc],Nodi)); % se il prossimo elemento della Lista Corrente è nullo
 trovaNodoSucc(Pos, Coda, ElementoDaCercare,[null0 - Lista|Acc],Nodi). % se Elemento \== ElementoDaCercare

% trovaNodoSucc(+ListaDiPianidiAzioneComuni, +ListaDiPianiDiAzione,+ElementoDaCercare, +Accumulatore, -NodiInComune)
% ritorna tutte liste comuni fino a una certa azione, che non abbiano come successore una variabile o il termine del piano d'azione
combinazioni([], Acc, Acc):-!.
combinazioni([Successore - Lista|Coda], Acc, Liste):-
(Successore \== null1,
 Successore \== null0,!,
 combinazioni(Coda, [ Lista| Acc], Liste),!; combinazioni(Coda,Acc,Liste)).

% listeDiverse(+ListaDiPianidiAzioneComuni, +Accumulatore ,-Liste)
% ritorna tutte liste che non entrano in un certo nodo
listeDiverse([], Acc, Acc):-!.
listeDiverse([Successore - Lista|Coda], Acc, Liste):-
(Successore==null0,!,
 listeDiverse(Coda, [ Lista| Acc], Liste); listeDiverse(Coda,Acc,Liste)).

% trovaElementi(+Posizione, +ListaDiPianiDiAzione, +Accumulatore, -Grafo)
% consente di creare la rete, ossia il grafo orientato per il calcolo del percorso minimo
trovaElementi(_, [], _, []):-!.
trovaElementi(Pos, [Lista|Coda], Acc, X):-
P is Pos + 1,
 (nth0(Pos, Lista, ElementoDaCercare),!, % estrae da Lista il Pos elemento da cercare (1,2,..)
 trovaNodoSucc(Pos, [Lista|Coda], ElementoDaCercare, [], Nodi), % restituisce tutti i piani d'azione.. vede se nella posizione Pos si trova lo stesso tipo di azione
 combinazioni(Nodi, [], Liste), % si estraggono le liste con l'azione ElementoDaCercare in comune
 trovaElementi(P, Liste, [ElementoDaCercare|Acc], M1), % crea nuovi nodi a partire dal nodo in comune ElementoDaCercare
 listeDiverse(Nodi,[], ListeDiverse), % cerca i piani d'azione che non entrano nel nodo ElementoDaCercare
 trovaElementi(Pos, ListeDiverse, Acc, M2), % crea nuovi nodi a partire dal nodo attuale
 X = [ElementoDaCercare + NN - M1, M2],!; % ElementoDaCercare + Variabile Non Istanziata - Nodi Adiacenti | Alternative
 trovaElementi(Pos, Coda, Acc, X)).

% trovaNodiAlternativi(+NodoCorrente, +Accumulatore, -Nodi)
% trova Nodi che fanno parte di percorsi alternativi
trovaNodiAlternativi([], Acc, Acc):-!.
trovaNodiAlternativi([El + C1 + C2 - M1,M2|Coda],Acc,X):-
 trovaNodiAlternativi(M2, [El|Acc],X).

%associa a ogni elemento una variabile Z >= 0
aggiornaLista([],Acc,Acc):-!.
aggiornaLista([T|C],Acc, NuovaLista):-
 Z #>= 0,
 aggiornaLista(C, [Z|Acc],NuovaLista).

% descriviRete(+Rete, -Obiettivi, -Risorse, -Successori, -ListaCompleta, +Predecessore, +Var)
% IMPOSTA I VINCOLI SUGLI ARCHI DELLA RETE
% descrive la rete
% Successori => sono i nodi finali del grafo, che vanno al nodo destinazione
% Obiettivi => variabili che valgono {0,1} * (Costo Nodo)
% ListaRisorse => variabili che valgono {0,1} * (Consumo Risorsa del Nodo)
% Var è la variabile che rappresenta il nodo coda dell'arco.. es. [a=>{b,c,d}], se selezioniamo a, b,c,o d, verranno selezionati
descriviRete([],[],[],[],_,_):-!.
%, writeln("nessuno/a").
descriviRete([El + C1 + C2 + J - M1,M2|_], Obiettivi, Risorse, Successori, Predecessore, Var):-
   trovaNodiAlternativi(M2, [],Y),
   append([El], Y, Y1),
    (Predecessore=Al - Lista,Alternativa is Al + 1, NuovaLista=Lista,!;
    Alternativa=1, aggiornaLista(Y1,[], NuovaLista)), %alternativa=1, significa sono nodi di arrivo da un nodo comune, es. dal nodo sorgente
   (Predecessore=Al - Lista, !;
   Predecessore=0, sum(NuovaLista, #=, 1) , !; % vincolo nodo sorgente
    K #= Var * (-1), append(NuovaLista,[K],NuovaLista0), sum(NuovaLista0, #=, 0)), % vincolo nodi di transito, Var rappresenta il nodo coda
 %  write(El),write(" ha come successori "),
   (Predecessore=Al - Lista, nth0(Al, Lista, Succ) ,!; nth0(0, NuovaLista, Succ)),
   J = Succ, % J=1 se il nodo corrente verrà selezionato nel percorso
   Z #= C1 * Succ, % vincolo percorso
   R #= C2 * Succ, % vincolo risorsa
        (M1=[], Succ1=[J],R1=[], Z1=[],
      %  writeln("nessuno/a"),
        !; % nodo finale
   descriviRete(M1, Z1, R1, Succ1, 1, Succ)),
 %  write(El),write(" ha come alternative "),
   descriviRete(M2, Z2, R2, Succ2, Alternativa - NuovaLista, _),
   append(Succ1,Succ2,Successori),
   (Predecessore=0, sum(Successori, #=, 1),!;true), %vincolo nodo destinazione
   append(Z1,Z2,Z3),
   append([Z],Z3,Obiettivi),
    append(R1,R2,R3),
   append([R],R3,Risorse),!.

% ricostruisciPiano(+Rete, -PianoOrdinatodiAzioni)
% ritorna il piano ordinato di azioni selezionate nel grafo
ricostruisciPiano([], []):-!.
ricostruisciPiano([El + C1 + C2 + J - M1,M2|_], Piano):-
  (J=1, Piano0=[El],!; Piano0=[]),
  ricostruisciPiano(M1, Piano1),
  ricostruisciPiano(M2, Piano2),
  append(Piano0, Piano1, Piano3),
  append(Piano3, Piano2, Piano).

% risolviRicercaOperativa(+ListaPianidiAzione, +VincoloRisorsa)
% a partire dalle possibili liste di piani d'azione trovati, calcola la soluzione al minor costo che rispetta i vincoli di risorsa
risolviRicercaOperativa(Liste, VincoloRisorsa):-
trovaElementi(0, Liste, [], X), %crea la rete
descriviRete(X, Obiettivi,Risorse, _, 0,_ ),
sum(Risorse, #=<, VincoloRisorsa),
sum(Obiettivi, #= , U),
labeling([min(U)], Obiettivi),
ricostruisciPiano(X,Piano),
write("costo finale"), writeln(U),
writeln("il piano è il seguente===>"),
scrivi(Piano).


% per semplicità supponiamo ogni azione consumi 1 risorsa
calcola_risorsa(_,1).

%% trova la sequenza di percorsi a costo minimo, che rispettano il vincolo di risorsa
testRicercaOperativa :- % ==================================================================
 statoIniziale(S),
 goal(G),
 sort(G, GO),
 findall(Piano, trovaPiani(S, [0-[GO,0,[],[],0]], 8, Piano), Liste),
 risolviRicercaOperativa(Liste,100).

%% trova la sequenza di percorsi a costo minimo tramite A*, che NON
%% NECESSARIAMENE è quella che rispetta il vincolo di risorsa
test(Piano):-
	statoIniziale(S),
	goal(G),
	sort(G, GO),
  pianifica(S, [0 - [GO,0,[],[], 0]], 15, Piano, Costo),
  write("costo finale "),
  writeln(Costo),
	scrivi(Piano).


% calcola_euristica(+S1,+S2,+DeleteList, +Contr, -Costo)
% h(X) = 0 se X = Goal
calcola_euristica(S1,S2,DeleteList, Contr, Costo):-
lista_conseg_logica(S1,S2,DeleteList, Contr, _, NL),
setof(C, NL^(sottostima(NL, 0, C, [])), [Costo|_]),!.


% sottostima(+Lista, +Somma, -Costo, +AddList)
sottostima([],Somma,Somma,_):-!.
sottostima(Lista,Somma, Costo, AddList):-
 NuovaSomma is Somma + 1,
 operatore(_,_,_,D),
 lista_conseg_logica(Lista, D, false, false, _, NCL), %ottengo una prima lista dei goal non soddisfatti dall'operatore selezionato
 %write("ncl è ==>"), write(NCL),write("d è ===> "), writeln(D),write("la add list è ==>"),writeln(AddList),
 \+(verificato(D, AddList)), % serve per evitare di ispezionare nuovamente lo stesso operatore
 (NCL=Lista, !, sottostima(Lista, NuovaSomma, Costo,[D|AddList]); % se tutta la lista non è conseguenza logica, si prova un altro operatore
 sottostima(NCL, NuovaSomma, Costo, [D])). % altrimenti rilancia il predicato con le nuove non conseguenze logiche

% verifica che la prima Lista non sia un sottoinsieme della seconda
verificato([],_) :- !,false.
verificato(_,[]) :- !,false.
verificato(Lista,[Testa|_]):-
 Lista=Testa,!.
verificato(Lista,[_|Coda]):-
 verificato(Lista,Coda).



calcola_costo(Azione, Costo):-
 costo(Azione,Costo),
 !.

%% VERIFICA CHE NON ABBIAMO VISITATO GIA' ALTRI GOALS
esaminati(GO,[T|_]) :-
  ( ground(GO),
    conseg_logica(T,GO,false)),
  !.
esaminati(GO,[_|C]) :-
	esaminati(GO,C).

%trovaPiani(+Stato,+SequenzaAzioni,+Piano)
trovaPiani(Stato, [_-[GO,Costo,SeqGoals,Piano,_]|_], _, Piano):-
  conseg_logica(GO, Stato,false).

% trovaPiani(+Stato,+SequenzaAzioni,+ProfonditaMax, -Piano)
% ProfonditaMax a partire da un certo nodo
% A rappresenta il valore di A* del nodo attuale
% algoritmo A*
trovaPiani(Stato, [A-P|Azioni], ProfonditaMax, Piano)  :-
 selezionaPiani(Stato,A-P, ProfonditaMax, AzioniEstese),
 mergeSort(AzioniEstese,Azioni,NuoveAzioni),
% estendiSequenze(TutteLeSequenze, AzioniEstese, NuoveSequenze ),
 trovaPiani(Stato, NuoveAzioni, ProfonditaMax, Piano).

% selezionaPiani(+StatoIniziale,+SequenzaAzioni,+ProfonditaMax, -AzioniEstese)
selezionaPiani(StatoIniziale, _-[Goal, CostoIniziale,Seq,Piano,P], ProfonditaMax, AzioniEstese ):-
    findall(A - [GO,CC,[GO|Seq],[Azione + Costo + Risorse|Piano], PP], % dal nodo migliore espande tutti i possibili nuovi nodi
 (member(G,Goal),operatore(Azione,Prec,Del,Add), % tali per cui esiste un operatore che,
    \+ inutile(Azione,Piano),
     conseg_logica([G],Add,false), %compiendo l'azione Azione, soddisfa almeno uno dei goals
     PP is P + 1, % si verifica non stiamo oltre la profondità massima consentita
     PP =< ProfonditaMax,%writeln(PP),
 % writeln(Azione|Piano),
     subtract(Goal,[G], GoalsRimanenti),
     verifica_rimanenti(GoalsRimanenti, Add, [], Lista), % verifica se altri Goals sono conseguenza logica del nuovo stato dove ci troveremo
     subtract(GoalsRimanenti, Lista, GoalsRegrediti0),
    non_contraddizione(GoalsRegrediti0, Prec,false), % verifica che i Goal non soddisfatti e le Precondizioni dell'Operatore non creino uno stato irrangiungibile
   % non_contraddizione(Prec, GoalsRegrediti0,false),
     \+ contraddizione(GoalsRegrediti0,Del,true), % verifica che i Goal non soddisfatti non vengano cancellati dagli obiettivi da raggiungere
     append(GoalsRegrediti0, Prec, GoalsRegrediti),
     sort(GoalsRegrediti, GO),
     \+ esaminati(GO,Seq), % evita cicli nel grafo => DA MIGLIORARE
     calcola_euristica(Prec, StatoIniziale,false,false,Euristica),
     calcola_costo(Azione,Costo),
     CC is Costo + CostoIniziale,
     A is CC + Euristica,
     calcola_risorsa(Azione, Risorse)
     ),Lista),
     sort(Lista, Sorted0),
     keysort(Sorted0, AzioniEstese).


%% ORDINA LE AZIONI PER COSTO CRESCENTE
mergeSort([],L,L).
mergeSort(L,[],L).
mergeSort([A1-P1|CP1],[A2-P2|CP2],[A1-P1|Coda]) :-	A1 =< A2,
	mergeSort(CP1,[A2-P2|CP2],Coda).
mergeSort([A1-P1|CP1],[A2-P2|CP2],[A2-P2|Coda]) :-
	A1 > A2,
	mergeSort([A1-P1|CP1],CP2,Coda).



pianifica(Stato, [A-[GO,Costo,SeqGoals,Piano,_]|_], _, Piano, Costo):-
  conseg_logica(GO, Stato,false).

pianifica(Stato, [A-P|Azioni], ProfonditaMax, Piano, CostoFinale)  :-
 seleziona(Stato,A-P, ProfonditaMax, AzioniEstese),
 mergeSort(AzioniEstese,Azioni,NuoveAzioni),
% estendiSequenze(TutteLeSequenze, AzioniEstese, NuoveSequenze ),
 pianifica(Stato, NuoveAzioni, ProfonditaMax, Piano, CostoFinale).



seleziona(StatoIniziale, _-[Goal, CostoIniziale,Seq,Piano,P], ProfonditaMax, AzioniEstese ):-
    findall(A - [GO,CC,[GO|Seq],[Azione|Piano], PP],
 (member(G,Goal),operatore(Azione,Prec,Del,Add),
  \+ inutile(Azione,Piano),
     conseg_logica([G],Add,false),
     PP is P + 1,
     PP =< ProfonditaMax,%writeln(PP),
  writeln(Azione|Piano),
     subtract(Goal,[G], GoalsRimanenti),
     verifica_rimanenti(GoalsRimanenti, Add, [], Lista),
     subtract(GoalsRimanenti, Lista, GoalsRegrediti0),
    non_contraddizione(GoalsRegrediti0, Prec,false),
  %  non_contraddizione(Prec, GoalsRegrediti0,false),
     \+ contraddizione(GoalsRegrediti0,Del,true),
     append(GoalsRegrediti0, Prec, GoalsRegrediti),
     sort(GoalsRegrediti, GO),
     \+ esaminati(GO,Seq),
     calcola_euristica(Prec, StatoIniziale,false,false,Euristica),
     calcola_costo(Azione,Costo),
     CC is Costo + CostoIniziale,
     A is  CC + Euristica
     ),Lista),
     sort(Lista, Sorted0),
     keysort(Sorted0, AzioniEstese).



:- set_prolog_flag(answer_write_options,[max_depth(0)]).


scrivi([T|C]) :- write(T),nl,
	scrivi(C).
scrivi([]).

