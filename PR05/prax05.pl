laevaga(tallinn, helsinki, 120, time(12, 45, 0.0), time(14, 45, 0.0)).
laevaga(tallinn, stockholm, 480, time(10, 30, 0.0), time(23, 25, 0.0)).
bussiga(tallinn, riia, 300, time(15, 0, 0.0), time(19, 30, 0.0)).
rongiga(riia, berlin, 680, time(8, 45, 0.0), time(13, 10, 0.0)).
lennukiga(tallinn, helsinki, 30, time(7, 45, 0.0), time(10, 40, 0.0)).
lennukiga(helsinki, paris, 180, time(11, 35, 0.0), time(14, 0, 0.0)).
lennukiga(paris, berlin, 120, time(17, 15, 0.0), time(20, 0, 0.0)).
lennukiga(paris, tallinn, 100, time(15, 50, 0.0), time(19, 5, 0.0)).

:- dynamic labitud/1.

transport(From, To, Price, Time1, Time2):-
    laevaga(From, To, Price, Time1, Time2) ;
    bussiga(From, To, Price, Time1, Time2) ;
    rongiga(From, To, Price, Time1, Time2) ;
    lennukiga(From, To, Price, Time1, Time2).

transport_name(From, To, Price, Time1, Time2, Name):-
    laevaga(From, To, Price, Time1, Time2), Name = laevaga ;
    bussiga(From, To, Price, Time1, Time2), Name = bussiga ;
    rongiga(From, To, Price, Time1, Time2), Name = rongiga ;
    lennukiga(From, To, Price, Time1, Time2), Name = lennukiga.

reisi(From, To, mine(From, Between, Pred, Path), Price):-
    transport_name(From, Between, Price1, _, _, Pred),
    reisi(Between, To, Path, Price2),
    Price is Price1 + Price2.
reisi(From, To, mine(From, To, Pred), Price):-
    transport_name(From, To, Price, _, _, Pred).

odavaim_reis(From, To, Path, Price):-
    findall(Price, reisi(From, To, Path, Price), List),
    min(Price, List).

odavaim_reis(From, To, Path, Price):- .
