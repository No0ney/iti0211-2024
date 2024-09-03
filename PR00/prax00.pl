lind(kotkas).
lind(jaanalind).
lind(hani).
lind(part).
oskab_lennata(kotkas).
oskab_lennata(hani).
oskab_lennata(part).
lendab(X):- lind(X), oskab_lennata(X).