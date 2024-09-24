viimane_element(Last, [Last|[]]).
viimane_element(Last, [_|Tail]):- viimane_element(X, [Tail]).

suurim([], []).
suurim([Head|[]], [X|Y]):- append(Head, Y, X).
suurim([Head, _ | []], [X|Y]):- append(Head, Y, X).
suurim([Head1, Head2 | Tail], [X|Y]):-
((Head1 > Head2, append(Head1, Y, X)) ; (Head1 < Head2, append(Head2, Y, X))), suurim(Tail, X).

%paki([Head | Tail], [X|Y]):- .
