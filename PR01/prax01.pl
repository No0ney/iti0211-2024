mother(emma, jane).
mother(carol, jane).
mother(harry, jane).

mother(jake, emma).
mother(lily, emma).

mother(kate, carol).
mother(simon, carol).

mother(may, megan).

mother(gary, may).
mother(mary, may).

married(jane, bob).
married(emma, alan).
married(carol, oliver).
married(megan, rob).
married(may, harry).

male(bob).
male(rob).
male(harry).
male(alan).
male(oliver).
male(jake).
male(simon).
male(gary).

female(jane).
female(carol).
female(emma).
female(megan).
female(may).
female(lily).
female(kate).
female(mary).

father(Child, Father):- married(Wife, Father), mother(Child, Wife).
brother(Child, Brother):- Child \= Brother, mother(Child, Mom), male(Brother), mother(Brother, Mom).
sister(Child, Sister):- Child \= Sister, mother(Child, Mom), female(Sister), mother(Sister, Mom).
aunt(Child, Aunt):- Child \= Aunt, mother(Child, Mom), sister(Mom, Aunt).
uncle(Child, Uncle):- mother(Child, Mom), brother(Mom, Uncle).
grandfather(Child, Grandfather):- mother(Child, Mom), mother(Mom, Grandma), married(Grandma, Grandfather).
grandmother(Child, Grandmother):- mother(Child, Mom), mother(Mom, Grandmother).
