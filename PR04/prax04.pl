laevaga(tallinn, helsinki, 120).
laevaga(tallinn, stockholm, 480).

bussiga(tallinn, riia, 300).

rongiga(riia, berlin, 680).

lennukiga(tallinn, helsinki, 30).
lennukiga(helsinki, paris, 180).
lennukiga(paris, berlin, 120).
lennukiga(paris, tallinn, 120 ).

:- dynamic labitud/1.

transport(From, To, Time):-
    laevaga(From, To, Time) ;
    bussiga(From, To, Time) ;
    rongiga(From, To, Time) ;
    lennukiga(From, To, Time).

transport_name(From, To, Time, Name):-
    laevaga(From, To, Time) -> Name = laevaga ;
    bussiga(From, To, Time) -> Name = bussiga ;
    rongiga(From, To, Time) -> Name = rongiga ;
    lennukiga(From, To, Time) -> Name = lennukiga.

reisi(From, To):-
    transport(From, To, _).
reisi(From, To):-
    transport(From, Between, _), reisi(Between, To).

reisi(From, To, mine(From, Between, Path)):-
    transport(From, Between, _),
    reisi(Between, To, Path).
reisi(From, To, mine(From, To)):-
    transport(From, To, _).

reisi_transpordiga(From, From, _):- !.
reisi_transpordiga(From, To, mine(From, Between, Pred, Path)):-
    transport_name(From, To, _, Pred),
    reisi_transpordiga(Between, To, Path).
reisi_transpordiga(From, To, mine(From, To, Pred)):-
    transport_name(From, To, _, Pred).

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
