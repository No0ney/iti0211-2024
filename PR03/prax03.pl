viimane_element(Last, [Last | []]).
viimane_element(Last, [_ | Tail]):- viimane_element(X, [Tail]).

suurim([], []).
suurim([El], [El | _]).
suurim([El1, El2 | Tail], [Max | Y]):-
    (El1 >= El2, Max = El1, suurim([El2 | Tail], Y)) ;
    (El1 < El2, Max = El2, suurim([El2 | Tail], Y)), !.

paki([], []).
paki([El], [El]).
paki([El1, El2 | Tail], [El1 | Y]):- El1 \= El2, paki([El2 | Tail], Y), !.
paki([_, El2 | Tail], X):- paki([El2 | Tail], X), !.

duplikeeri([], _).
duplikeeri([El], [El, El | _]):- !.
duplikeeri([El1, El2 | Tail], [El1, El1 | Y]):- duplikeeri([El2 | Tail], Y), !.

add_el(_, 0, []).
add_el(L, N, X):- append([L], P, X), add_el(L, M, P), N is M + 1.

kordista([], _, []).
kordista(L, 1, L).
kordista([El1 | Tail], N, X):- append(L, P, X), add_el(El1, N, L), kordista(Tail, N, P).

paaritu_vordle([], _, _).
paaritu_vordle([El | Tail], List, X):-
    (mod(El, 2) =\= 0, append(List, [El], New),
    ((Tail = [], X = New) ; paaritu_vordle(Tail, New, X))) ;
    (mod(El, 2) =:= 0,
    ((Tail = [], X = List) ; paaritu_vordle(Tail, List, X))).

paaris_vordle([], _, _).
paaris_vordle([El | Tail], List, X):-
    (mod(El, 2) =:= 0, append(List, [El], New),
    ((Tail = [], X = New) ; paaris_vordle(Tail, New, X))) ;
    (mod(El, 2) =\= 0,
    ((Tail = [], X = List) ; paaris_vordle(Tail, List, X))).

suurem_vordle([], _, _, _).
suurem_vordle([El | Tail], N, List, X):-
    (El > N, append(List, [El], New),
    ((Tail = [], X = New) ; suurem_vordle(Tail, N, New, X))) ;
    ((El < N ; El =:= N),
    ((Tail = [], X = List) ; suurem_vordle(Tail, N, List, X))).

vordle_predikaadiga([], _, []).
vordle_predikaadiga(List, [Pred | N], X):-
    (Pred == "paaritu_arv", paaritu_vordle(List, [], X)) ;
    (Pred == "paaris_arv", paaris_vordle(List, [], X)) ;
    (Pred == "suurem_kui", suurem_vordle(List, N, [], X)).