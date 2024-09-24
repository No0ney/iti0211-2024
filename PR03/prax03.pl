viimane_element(X, []).
viimane_element(X, [Head|Tail]):- viimane_element(Head, [Tail]).
