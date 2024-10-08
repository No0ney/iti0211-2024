laevaga(tallinn, helsinki, 120).
laevaga(tallinn, stockholm, 480).

bussiga(tallinn, riia, 300).

rongiga(riia, berlin, 680).

lennukiga(tallinn, helsinki, 30).
lennukiga(helsinki, paris, 180).
lennukiga(paris, berlin, 120).
lennukiga(paris, tallinn, 120 ).

reisi(From, To):-
    laevaga(From, To, _) ;
    bussiga(From, To, _) ;
    rongiga(From, To, _) ;
    lennukiga(From, To, _).
reisi(From, To):-
    (laevaga(From, Between, _) ;
    bussiga(From, Between, _) ;
    rongiga(From, Between, _) ;
    lennukiga(From, Between, _)), reisi(Between, To).

reisi(From, From, _):- !.
reisi(From, To, mine(From, Between, Path)):-
    (laevaga(From, Between, _) ;
    bussiga(From, Between, _) ;
    rongiga(From, Between, _) ;
    lennukiga(From, Between, _)),
    reisi(Between, To, Path).
reisi(From, To, mine(From, To)):-
    laevaga(From, To, _) ;
    bussiga(From, To, _) ;
    rongiga(From, To, _) ;
    lennukiga(From, To, _).

reisi_transpordiga(From, From, _):- !.
reisi_transpordiga(From, To, mine(From, Between, Pred, Path)):-
    ((laevaga(From, Between, _), Pred = laevaga) ;
    (bussiga(From, Between, _), Pred = bussiga) ;
    (rongiga(From, Between, _), Pred = rongiga) ;
    (lennukiga(From, Between, _), Pred = lennukiga)),
    reisi_transpordiga(Between, To, Path).
reisi_transpordiga(From, To, mine(From, To, Pred)):-
    (laevaga(From, To, _), Pred = laevaga) ;
    (bussiga(From, To, _), Pred = bussiga) ;
    (rongiga(From, To, _), Pred = rongiga) ;
    (lennukiga(From, To, _), Pred = lennukiga).

reisi(From, To, mine(From, Between, Pred, Path), Time):-
    (laevaga(From, Between, Time1) -> Pred = laevaga ;
    bussiga(From, Between, Time1) -> Pred = bussiga ;
    rongiga(From, Between, Time1) -> Pred = rongiga ;
    lennukiga(From, Between, Time1) -> Pred = lennukiga),
    reisi(Between, To, Path, Time2),
    Time is Time1 + Time2.
reisi(From, To, mine(From, To, Pred), Time):-
    (laevaga(From, To, Time1) -> Pred = laevaga ;
    bussiga(From, To, Time1) -> Pred = bussiga ;
    rongiga(From, To, Time1) -> Pred = rongiga ;
    lennukiga(From, To, Time1) -> Pred = lennukiga),
    Time is Time1.
