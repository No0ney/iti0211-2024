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

mother(oliver, sam).
mother(pam, sam).
mother(hunter, sam).

mother(amy, rose).

mother(emily, amy).
mother(maru, amy).
mother(sebastian, amy).

married(jane, bob).
married(emma, alan).
married(carol, oliver).
married(megan, rob).
married(may, harry).
married(sam, neil).
married(rose, richard).
married(amy, hunter).

male(bob).
male(rob).
male(harry).
male(alan).
male(oliver).
male(jake).
male(simon).
male(gary).
male(neil).
male(hunter).
male(richard).

female(jane).
female(carol).
female(emma).
female(megan).
female(may).
female(lily).
female(kate).
female(mary).
female(sam).
female(pam).
female(rose).
female(amy).

father(Child, Father):- married(Wife, Father), mother(Child, Wife).
brother(Child, Brother):- Child \= Brother, mother(Child, Mom), male(Brother), mother(Brother, Mom).
sister(Child, Sister):- Child \= Sister, mother(Child, Mom), female(Sister), mother(Sister, Mom).
aunt(Child, Aunt):- Child \= Aunt, mother(Child, Mom), (sister(Mom, Aunt) ; (married(Mom, Dad), sister(Dad, Aunt))).
uncle(Child, Uncle):- mother(Child, Mom), (brother(Mom, Uncle) ; (married(Mom, Dad), brother(Dad, Uncle))).
grandfather(Child, Grandfather):- mother(Child, Mom), (mother(Mom, Grandma) ; (married(Mom, Dad), mother(Dad, Grandma))), married(Grandma, Grandfather).
grandmother(Child, Grandmother):- mother(Child, Mom), (mother(Mom, Grandmother) ; (married(Mom, Dad), mother(Dad, Grandmother))).

parent(Child, Parent):- mother(Child, Parent) ; father(Child, Parent).

ancestor(Child, Parent):- parent(Child, Parent).
ancestor(Child, Parent):- parent(Child, X), ancestor(X, Parent).

male_ancestor(Child, Parent):- father(Child, Parent).
male_ancestor(Child, Parent):- father(Child, X), male_ancestor(X, Parent).

female_ancestor(Child, Parent):- mother(Child, Parent).
female_ancestor(Child, Parent):- mother(Child, X), female_ancestor(X, Parent).

ancestor1(Child, Parent, N):- N \= 0, (mother(Parent, Between) ; father(Parent, Between)), ancestor1(Child, Between, N - 1).

%ancestor2(Child, Parent, X):- .
