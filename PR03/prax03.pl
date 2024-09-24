viimane_element(Last, [Last|[]]).
viimane_element(Last, [_|Tail]):- viimane_element(X, [Tail]).

suurim([], []).
suurim([X], [X]).
%suurim([Head1, Head2 | Tail], [X|Y]):- .

paki([], []).
paki([X], [X]).
paki([El1, El2 | Tail], [X|Y]):- El1 \= El2, paki([El2 | Tail], [El1 | Y]).
paki([El1, El2 | Tail], [X|Y]):- paki([El2 | Tail], [X|Y]).
