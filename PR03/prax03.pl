viimane_element(Last, [Last|[]]).
viimane_element(Last, [_|Tail]):- viimane_element(X, [Tail]).

suurim([], []).
suurim([El], [El]).
%suurim([Head1, Head2 | Tail], [X|Y]):- .

paki([], []).
paki([El], [El]).
paki([El1, El2 | Tail], [_|Y]):- El1 \= El2, paki([El2 | Tail], [El1 | Y]).
paki([El1, El2 | Tail], [X|Y]):- paki([El2 | Tail], [X|Y]).

duplikeeri([], []).
duplikeeri([El], [El]).
duplikeeri([El1, El2 | Tail], [X|Y]):- duplikeeri([El2 | Tail], [El1, El1 | Y]).

kordista([], X, []).

vordle_predikaadiga([], [], []).