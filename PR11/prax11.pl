employee_db_1('John Doe', 30, 'Software Engineer').
employee_db_1('Jane Smith', 25, 'Data Scientist').
employee_db_1('Bob Johnson', 35, 'Project Manager').
employee_db_1('Alice Brown', 28, 'QA Engineer').
employee_db_1('John Johnson', 35, 'Software Engineer').

employee_db_2('Eva Williams', 32, 'UX Designer').
employee_db_2('Michael Davis', 28, 'Systems Analyst').
employee_db_2('Sarah Jones', 33, 'Business Analyst').

yhisosa([], _, []).
yhisosa([X], [X], [X]).
yhisosa([El | Tail], List2, [El | X]):-
    member(El, List2),
    yhisosa(Tail, List2, X).
yhisosa([El | Tail], List2, X):-
    not(member(El, List2)),
    yhisosa(Tail, List2, X).

yhend(Result, [], Result).
yhend([X], [X], [X]).
yhend(List, [El | Tail], Result):-
    not(member(El, List)),
    append(List, [El], NewList),
    yhend(NewList, Tail, Result).
yhend(List, [El | Tail], Result):-
    member(El, List),
    yhend(List, Tail, Result).

vahe([], _, []).
vahe([X], [X], []).
vahe([El | Tail], List, [El | X]):-
    not(member(El, List)),
    vahe(Tail, List, X).
vahe([El | Tail], List, X):-
    member(El, List),
    vahe(Tail, List, X).

ristkorrutis(List1, List2, Result):-
    findall([El1, El2], (member(El1, List1), member(El2, List2)), Result).

name(EmployeeDB, Criteria, Result):-
    findall([Name, Age, Position],
            (call(EmployeeDB, Name, Age, Position),
            Name = Criteria),
            Result).

age(EmployeeDB, Criteria, Result):-
    findall([Name, Age, Position],
            (call(EmployeeDB, Name, Age, Position),
            Age = Criteria),
            Result).

position(EmployeeDB, Criteria, Result):-
    findall([Name, Age, Position],
            (call(EmployeeDB, Name, Age, Position),
            Position = Criteria),
            Result).

find_employee(EmployeeDB, Attribute, Criteria, Result):-
    current_predicate(EmployeeDB/3),
    Attribute = 'Name' -> name(EmployeeDB, Criteria, Result) ;
    Attribute = 'Age' -> age(EmployeeDB, Criteria, Result) ;
    Attribute = 'Position' -> position(EmployeeDB, Criteria, Result).
