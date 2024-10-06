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

%paaritu_vordle([El], )
paaritu(N): N mod 2 =:= 1.
paaritu_vordle([El | Tail], [X|Y]):- (paaritu(El), X = El) ; paaritu_vordle(Tail, Y).

paaris(N): not(paaritu(N)).
paaris_vordle([El | Tail], [X|Y]):- (paaris(El), X = El) ; paaris_vordle(Tail, Y).

suurem_vordle([El | Tail], N, [X|Y]):- (El > N, X = El) ; suurem_vordle(Tail, N, Y).

vordle_predikaadiga(List, [Pred | N], X):-
    (Pred == "paaritu_arv", paaritu_vordle(List, X)) ;
    (Pred == "paaris_arv", paaris_vordle(List, X)) ;
    (Pred == "suurem_kui(X)", suurem_vordle(List, N, X)).