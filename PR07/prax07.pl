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

:- dynamic max/3.

find_terminal(Node, Node):- \+ is_a(_, Node).
find_terminal(Node, Terminal):-
    is_a(Between, Node),
    find_terminal(Between, Terminal).

count_terminals(Node, Terminals, Count):-
    findall(Terminal, find_terminal(Node, Terminal), Terminals),
    length(Terminals, Count).

extintion(Who, What_spieces, How_many):- true.

find_most_sensitive_species(Died, Count, Species):- true.
