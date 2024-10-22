laevaga(tallinn, helsinki, 120, time(12, 45, 0.0), time(14, 45, 0.0)).
laevaga(tallinn, stockholm, 480, time(10, 30, 0.0), time(23, 25, 0.0)).
bussiga(tallinn, riia, 300, time(15, 0, 0.0), time(19, 30, 0.0)).
rongiga(riia, berlin, 680, time(8, 45, 0.0), time(13, 10, 0.0)).
lennukiga(tallinn, helsinki, 30, time(7, 45, 0.0), time(10, 40, 0.0)).
lennukiga(helsinki, paris, 180, time(11, 35, 0.0), time(14, 0, 0.0)).
lennukiga(paris, berlin, 120, time(17, 15, 0.0), time(20, 0, 0.0)).
lennukiga(paris, tallinn, 100, time(15, 50, 0.0), time(19, 5, 0.0)).

:- dynamic labitud/1.

transport(From, To, Price):-
    laevaga(From, To, Price, _, _) ;
    bussiga(From, To, Price, _, _) ;
    rongiga(From, To, Price, _, _) ;
    lennukiga(From, To, Price, _, _).

transport_name(From, To, Price, Name):-
    laevaga(From, To, Price, _, _), Name = laevaga ;
    bussiga(From, To, Price, _, _), Name = bussiga ;
    rongiga(From, To, Price, _, _), Name = rongiga ;
    lennukiga(From, To, Price, _, _), Name = lennukiga.

transport_name(From, To, Price, TimeFrom, TimeTo, Name):-
    laevaga(From, To, Price, TimeFrom, TimeTo), Name = laevaga ;
    bussiga(From, To, Price, TimeFrom, TimeTo), Name = bussiga ;
    rongiga(From, To, Price, TimeFrom, TimeTo), Name = rongiga ;
    lennukiga(From, To, Price, TimeFrom, TimeTo), Name = lennukiga.

reisi(From, To, mine(From, To, Pred), Price):-
    transport_name(From, To, Price1, Pred),
    Price is Price1.
reisi(From, To, mine(From, Between, Pred, Path), Price):-
    assertz(labitud(From)),
    transport_name(From, Between, Price1, Pred),
    not(labitud(Between)),
    reisi(Between, To, Path, Price2),
    Price is Price1 + Price2,
    retractall(labitud/1).

to_seconds(H, M, S, Seconds):-
    Seconds is H * 3600 + M * 60 + S.

from_seconds(Seconds, Aeg):-
    time(H, M, S) = Aeg,
    H is Seconds // 3600,
    M is Seconds mod 3600 // 60,
    S is Seconds mod 3600 mod 60.

aegade_vahe(Aeg1, Aeg2, Vahe):-
    time(H1,M1,S1) = Aeg1,
    time(H2,M2,S2) = Aeg2,
    to_seconds(H1, M1, S1, Sec1),
    to_seconds(H2, M2, S2, Sec2),
    Diff is abs(Sec1 - Sec2),
    from_seconds(Diff, Vahe).

reisi_aega(From, To, mine(From, To, Pred), Price, Time):-
    transport_name(From, To, Price1, TimeFrom, TimeTo, Pred),
    Price is Price1,
    aegade_vahe(TimeFrom, TimeTo, Time).
reisi_aega(From, To, mine(From, Between, Pred, Path), Price, Time):-
    assertz(labitud(From)),
    transport_name(From, Between, Price1, TimeFrom, TimeTo, Pred),
    not(labitud(Between)),
    aegade_vahe(TimeFrom, TimeTo, Time1),
    reisi_aega(Between, To, Path, Price2, Time2),
    Price is Price1 + Price2,
    Time is Time1 + Time2,
    retractall(labitud/1).

odavaim_reis(From, To, Path, Price):-
    findall([Price, Path], reisi(From, To, Path, Price), L),
    odavaim(L, Price, Path).

odavaim([El | Tail], Price, Path):-
    odavaim(Tail, El, Price, Path).

odavaim([], [MinPrice, MinPath], MinPrice, MinPath).
odavaim([[XPrice, XPath | []] | Tail], [MinPrice, MinPath], Price, Path):-
    (XPrice < MinPrice, odavaim(Tail, [XPrice, XPath], Price, Path)) ;
    (MinPrice < XPrice, odavaim(Tail, [MinPrice, MinPath], Price, Path)).

lyhim_reis(From, To, Path, Price):-
    findall(Time, reisi_aega(From, To, Path, Price, Time), L),
    min_list(L, MinTime),
    reisi_aega(From, To, Path, Price, MinTime).
