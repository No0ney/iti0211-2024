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

reisi(From, From, _).
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
