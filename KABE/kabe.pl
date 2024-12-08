:- module('Anna Meier', [do_turn/3]).

do_turn(Color, X, Y):-
    possible_moves(Color, X, Y, List),
    choose_best_move(List, [0, 0, 0, 0, 0, 0], Move),
    do_move(Move),
    !.
do_turn(_, _, _).

% ===========================================================================
% Find possible moves first
% ===========================================================================

find_direction(1,1):- !.
find_direction(10,1).
find_direction(10,-1).
find_direction(2,-1).
find_direction(20,-1).
find_direction(20,1).

other_color(1, 2).
other_color(1, 20).
other_color(10, 2).
other_color(10, 20).
other_color(2, 1).
other_color(2, 10).
other_color(20, 1).
other_color(20, 10).

dame_color(1, 10).
dame_color(2, 20).

within_boarder(X):- member(X, [1, 2, 3, 4, 5, 6, 7, 8]).



possible_moves(Color, 0, 0, List):-
    find_direction(Color, Direction),
    dame_color(Color, DameColor),
    findall(ruut(X, Y, Color), ruut(X, Y, Color), MenRoots),
    findall(ruut(X, Y, DameColor), ruut(X, Y, DameColor), DameRoots),
    append(MenRoots, DameRoots, Roots),
    check_if_can_take_piece(Roots, Direction, TakeList),
    (length(TakeList, Len),
    (Len \= 0, append([], TakeList, List))
    ;
    check_change_position(Roots, Direction, MoveList),
    append([], MoveList, List)).

possible_moves(Color, X, Y, List):-
    find_direction(Color, Direction),
    check_if_can_take_piece([ruut(X, Y, Color)], Direction, TakeList),
    check_change_position([ruut(X, Y, Color)], Direction, MoveList),
    length(TakeList, Len),
    (Len \= 0, append([], TakeList, List)) ; append([], MoveList, List).

check_if_can_take_piece([], _, []).
check_if_can_take_piece([ruut(X, Y, Color) | Roots], Direction, TakeList):-
    (member(Color, [1, 2]) ->
    findall([X, Y, X1, Y1, X2, Y2],
    moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2),
    TakeList1)
    ;
    (member(Color, [10, 20]) ->
    findall([X, Y, X1, Y1, X2, Y2],
    moves_for_dames_take(X, Y, X1, Y1, X2, Y2),
    TakeList1))),
    check_if_can_take_piece(Roots, Direction, TakeList2),
    append(TakeList1, TakeList2, TakeList).

check_change_position([], _, []).
check_change_position([ruut(X, Y, Color) | Roots], Direction, MoveList):-
    (member(Color, [1, 2]) ->
    findall([X, Y, 0, 0, X1, Y1], moves_for_men_move(X, Y, Direction, X1, Y1), MoveList1)
    ;
    (member(Color, [10, 20]) ->
    findall([X, Y, 0, 0, X1, Y1], moves_for_dames_move(X, Y, X1, Y1), MoveList1))),
    check_change_position(Roots, Direction, MoveList2),
    append(MoveList1, MoveList2, MoveList).

check_dame(X, Y, Color):-
    ((Color = 1 ->
    (X = 8, retract(ruut(X, Y, Color)), assertz(ruut(X, Y, 10))))
    ;
    (Color = 2 ->
    (X = 1, retract(ruut(X, Y, Color)), assertz(ruut(X, Y, 20))))) ; true.

% ---------- MAN MOVES ----------

% Check up-left
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X + Direction, within_boarder(X1),
    Y1 is Y - 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
    X2 is X1 + Direction, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check up-right
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X + Direction, within_boarder(X1),
    Y1 is Y + 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
    X2 is X1 + Direction, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check down-left
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X - Direction, within_boarder(X1),
    Y1 is Y - 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
    X2 is X1 - Direction, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check down-right
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X - Direction, within_boarder(X1),
    Y1 is Y + 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
    X2 is X1 - Direction, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Simply move a man forward
moves_for_men_move(X, Y, Direction, X1, Y1):-
    ruut(X, Y, Color),
    X1 is X + Direction, within_boarder(X1),
    (Y1 is Y - 1 ; Y1 is Y + 1), within_boarder(Y1),
    ruut(X1, Y1, 0),

    % Check that there is no enemy piece immediately next to it with a blank space across
    (X2 is X1 + 1, Y2 is Y1 + 1, X3 is X1 - 1, Y3 is Y1 - 1
    ;
    X2 is X1 - 1, Y2 is Y1 + 1, X3 is X1 + 1, Y3 is Y1 - 1
    ;
    X2 is X1 + 1, Y2 is Y1 - 1, X3 is X1 - 1, Y3 is Y1 + 1
    ;
    X2 is X1 - 1, Y2 is Y1 - 1, X3 is X1 + 1, Y3 is Y1 + 1),
    within_boarder(X2), within_boarder(Y2),
    within_boarder(X3), within_boarder(Y3),
    ruut(X2, Y2, OtherColor),
    not(other_color(Color, OtherColor)),
    not(ruut(X3, Y3, 0)).

% ---------- DAME MOVES ----------

check_no_ally_between(X, Y, Color, X1, Y1):-
    (X > X1, Y > Y1 -> DirectionX is -1, DirectionY is -1
    ;
    X > X1, Y < Y1 -> DirectionX is -1, DirectionY is 1
    ;
    X < X1, Y > Y1 -> DirectionX is 1, DirectionY is -1
    ;
    X < X1, Y < Y1 -> DirectionX is 1, DirectionY is 1),
    check_no_ally_between_recursive(X, Y, Color, X1, Y1, DirectionX, DirectionY).

check_no_ally_between_recursive(X, Y, Color, X1, Y1, DirectionX, DirectionY):-
    NextX is X + DirectionX,
    NextY is Y + DirectionY,
    (NextX = X1, NextY = Y1 -> true
    ;
    not(ruut(NextX, NextY, Color)),
    within_boarder(NextX),
    within_boarder(NextY),
    check_no_ally_between_recursive(NextX, NextY, Color, X1, Y1, DirectionX, DirectionY)).

% Check up-left
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X + Spaces, within_boarder(X1),
    Y1 is Y - Spaces, within_boarder(Y1),
    check_no_ally_between(X, Y, Color, X1, Y1),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
%    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 + 1, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).
%    check_no_ally_between(X1, Y1, Color, AfterSpaces - 1, X2, Y2).

% Check up-right
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X + Spaces, within_boarder(X1),
    Y1 is Y + Spaces, within_boarder(Y1),
    check_no_ally_between(X, Y, Color, X1, Y1),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
%    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 + 1, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).
%    check_no_ally_between(X1, Y1, Color, AfterSpaces - 1, X2, Y2).

% Check down-left
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X - Spaces, within_boarder(X1),
    Y1 is Y - Spaces, within_boarder(Y1),
    check_no_ally_between(X, Y, Color, X1, Y1),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
%    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 - 1, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).
%    check_no_ally_between(X1, Y1, Color, AfterSpaces - 1, X2, Y2).

% Check down-right
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X - Spaces, within_boarder(X1),
    Y1 is Y + Spaces, within_boarder(Y1),
    check_no_ally_between(X, Y, Color, X1, Y1),
    ruut(X1, Y1, OtherColor),
    Color =\= OtherColor, OtherColor =\= 0,
%    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 - 1, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).
%    check_no_ally_between(X1, Y1, Color, AfterSpaces - 1, X2, Y2).

% Simply move dame whereever
moves_for_dames_move(X, Y, X1, Y1):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    (X1 is X + Spaces ; X1 is X - Spaces), within_boarder(X1),
    (Y1 is Y + Spaces ; Y1 is Y - Spaces), within_boarder(Y1),

    ruut(X1, Y1, 0),
    check_no_ally_between(X, Y, Color, Spaces - 1, X1, Y1),

    % Check that there is no enemy piece immediately next to it with a blank space across
    (X2 is X1 + 1, Y2 is Y1 + 1, X3 is X1 - 1, Y3 is Y1 - 1
    ;
    X2 is X1 - 1, Y2 is Y1 + 1, X3 is X1 + 1, Y3 is Y1 - 1
    ;
    X2 is X1 + 1, Y2 is Y1 - 1, X3 is X1 - 1, Y3 is Y1 + 1
    ;
    X2 is X1 - 1, Y2 is Y1 - 1, X3 is X1 + 1, Y3 is Y1 + 1),
    within_boarder(X2), within_boarder(Y2),
    within_boarder(X3), within_boarder(Y3),
    ruut(X2, Y2, OtherColor),
    not(other_color(Color, OtherColor)),
    not(ruut(X3, Y3, 0)).

% ===========================================================================
% Choose the best move
% ===========================================================================

choose_best_move([], Move, Move).
choose_best_move([[X, Y, X1, Y1, X2, Y2] | List],
                 [N, M, N1, M1, N2, M2], Move):-
    ruut(X, Y, Color),
    ((Color = 1 -> DameSpace is 8) ; (Color = 2 -> DameSpace is 1) ; true),
    ((N2 = 0, X2 \= 0 ; (N2 = 0 ; N2 \= 0), X2 = DameSpace ; N = 0, X \= 0) ->
    append([], [X, Y, X1, Y1, X2, Y2], Best)
    ;
    append([], [N, M, N1, M1, N2, M2], Best)),
    choose_best_move(List, Best, Move).

% ===========================================================================
% Do the best move
% ===========================================================================

do_move([X, Y, X1, Y1, X2, Y2]):-
    ruut(X, Y, Color),
    retract(ruut(X, Y, Color)),
    assertz(ruut(X, Y, 0)),
    (X1 \= 0 -> retract(ruut(X1, Y1, _)), assertz(ruut(X1, Y1, 0)) ; true),
    retract(ruut(X2, Y2, 0)),
    assertz(ruut(X2, Y2, Color)),
    check_dame(X2, Y2, Color).
