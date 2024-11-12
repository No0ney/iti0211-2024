is_a(roovloomad,elusolend).
is_a(mitte-roovloomad,elusolend).
is_a(veeimetajad,roovloomad).
is_a(kalad,roovloomad).
is_a(saarmas,veeimetajad).
is_a(kobras,veeimetajad).
is_a(ahven,kalad).
is_a(haug,kalad).
is_a(zooplankton,mitte-roovloomad).
is_a(veetaimed,mitte-roovloomad).
is_a(vesikatk,veetaimed).
is_a(vetikas,veetaimed).

eats(zooplankton,veetaimed).
eats(kalad,zooplankton).
eats(veeimetajad,kalad).

:- dynamic lastNode/1.

count_terminals(Node, Terminals, Count):- true.

extintion(Who,What_spieces,How_many):- true.

find_most_sensitive_species(Died, Count, Species):- true.

%    is_a(X, Node),
%    count_terminals(X, Terminals, Count),
%    count_terminals(Terminals, Count).
%count_terminals(Node, _, _):-
%    not(is_a(_, Node)),
%    assert(lastNode(Node)),
%    count_terminals(Terminals, Count).
%count_terminals(Terminals, Count):-
%    findall(Node, lastNode(Node), Terminals),
%    length(Terminals, Count).

%extinction(Who,What_spieces,How_many):-.