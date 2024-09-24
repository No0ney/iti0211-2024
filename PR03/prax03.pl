viimane_element(Last, [Last|[]]).
viimane_element(Last, [_|Tail]):- viimane_element(X, [Tail]).
