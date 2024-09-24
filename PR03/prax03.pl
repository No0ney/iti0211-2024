viimane_element(Last, [Last|[]]).
viimane_element(Last, [_|Tail]):- viimane_element(X, [Tail]).

suurim([], []).
suurim([El], [El]).
%suurim([Head1, Head2 | Tail], [X|Y]):- .

paki([], []).
paki([El], [El]).
paki([El1, El2 | Tail], [El1|Y]):- El1 \= El2, paki([El2 | Tail], Y), !.
paki([_, El2 | Tail], X):- paki([El2 | Tail], X), !.

duplikeeri([], []).
duplikeeri([El], [El]).
duplikeeri([El1, El2 | Tail], [X|Y]):- duplikeeri([El2 | Tail], [El1, El1 | Y]).

kordista([], X, []).

%paaritu_vordle([El], )
paaritu_vordle([El1, El2 | Tail], [X|Y]):- paaritu_vordle([_ | Tail], [El1, Y]).
paaris_vordle([El1, El2 | Tail], [X|Y]).
suurem_vordle([El1 | Tail], N, [X|Y]).
vordle_predikaadiga(List, [Pred | N], X):-
    (Pred = paaritu_arv, paaritu_vordle(List, X)) ;
    (Pred = paaris_arv, paaris_vordle(List, X)) ;
    (Pred = suurem_kui(X), suurem_vordle(List, N, X)).