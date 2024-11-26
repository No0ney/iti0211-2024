struktuur([maja(1, _, _, _, _, _),
            maja(2, _, _, _, _, _),
            maja(3, _, _, _, _, _),
            maja(4, _, _, _, _, _),
            maja(5, _, _, _, _, _)]).

next_to(A, B, Majad) :- append(_, [A, B|_], Majad).
next_to(A, B, Majad) :- append(_, [B, A|_], Majad).

joob_vett(_).
lemmikloomaks_kalad(_).

moistatus :-
    struktuur(Majad),

    member(maja(_, punane, inglane, _, _, _), Majad),
    member(maja(_, _, hispaanlane, _, _, koer), Majad),
    member(maja(_, roheline, _, kohvi, _, _), Majad),
    member(maja(_, _, ukrainlane, tee, _, _), Majad),
    next_to(maja(_, valge, _, _, _, _), maja(_, roheline, _, _, _, _), Majad),
    member(maja(_, _, _, _, winston, madu), Majad),
    member(maja(_, kollane, _, _, kool, _), Majad),
    member(maja(3, _, _, piim, _, _), Majad),
    member(maja(1, _, norralane, _, _, _), Majad),
    next_to(maja(_, _, _, _, chesterfield, _), maja(_, _, _, _, _, rebane), Majad),
    next_to(maja(_, _, _, _, kool, _), maja(_, _, _, _, _, hobune), Majad),
    member(maja(_, _, _, mahl, lucky_strike, _), Majad),
    member(maja(_, _, jaapanlane, _, kent, _), Majad),
    next_to(maja(_, _, norralane, _, _, _), maja(_, sinine, _, _, _, _), Majad),

    member(maja(_, _, Kes, vesi, _, _), Majad),
    joob_vett(Kes),
    member(maja(_, _, Kellel, _, _, kalad), Majad),
    lemmikloomaks_kalad(Kellel),

    write('Joob vett: '), write(Kes), nl,
    write('Lemmikloomaks kalad: '), write(Kellel), nl, !.
