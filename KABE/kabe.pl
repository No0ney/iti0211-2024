:- module('Anna Meier', [larry/3]).

larry(Color, X, Y):-
    possible_moves(Color, X, Y, List, Random),
    choose_best_move(Random, List, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0], Move),
    do_move(Move),
    !.
larry(_, _, _).

% ===========================================================================
% Find possible moves first.
% ===========================================================================

% Find which way a piece moves depending on the color.
find_direction(1,1):- !.
find_direction(10,1).
find_direction(10,-1).
find_direction(2,-1).
find_direction(20,-1).
find_direction(20,1).

% Find the opposite color of the piece.
other_color(1, 2).
other_color(1, 20).
other_color(10, 2).
other_color(10, 20).
other_color(2, 1).
other_color(2, 10).
other_color(20, 1).
other_color(20, 10).

% Find the color code for the dame.
dame_color(1, 8, 10).
dame_color(2, 1, 20).

% Check that the coord is not outside of the boards boundaries.
within_boarder(X):- member(X, [1, 2, 3, 4, 5, 6, 7, 8]).

% Check all pieces and what moves they can make.
possible_moves(Color, 0, 0, List, Random):-
    find_direction(Color, Direction),
    dame_color(Color, _, DameColor),
    findall(ruut(X, Y, Color), ruut(X, Y, Color), MenRoots),
    findall(ruut(X, Y, DameColor), ruut(X, Y, DameColor), DameRoots),
    append(MenRoots, DameRoots, Roots),
    % If TakeList is not empty, then no need to look for other moves.
    check_if_can_take_piece(Roots, Direction, TakeList),
    length(TakeList, TakeLen),
    ((TakeLen \= 0, append([], TakeList, List), Random is 0)
    ;
    % If Take list was empty, then find positions to which a piece can be moved without
    % the risk of being eaten.
    check_safe_change_position(Roots, Direction, SafeMoveList),
    length(SafeMoveList, MoveLen),
    ((MoveLen \= 0, append([], SafeMoveList, List), Random is 1)
    ;
    % If there are no more safe positions, then just find all the available ones.
    check_change_position(Roots, Direction, MoveList),
    append([], MoveList, List), Random is 1)).


% Check if the same piece that previously moved can make another move.
possible_moves(_, X, Y, List, Random):-
    ruut(X, Y, Color),
    find_direction(Color, Direction),
    check_if_can_take_piece([ruut(X, Y, Color)], Direction, TakeList),
    length(TakeList, TakeLen),
    TakeLen \= 0, append([], TakeList, List), Random is 0.


% Check if the piece can take other piece.
check_if_can_take_piece([], _, []).
check_if_can_take_piece([ruut(X, Y, Color) | Roots], Direction, TakeList):-
    % Check moves for men.
    (member(Color, [1, 2]) ->
    findall([X, Y, X1, Y1, X2, Y2, X3, Y3, X4, Y4],
    (moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2),
    % Check if later on the piece would be able to make another move after this one.
    check_for_men_take_again(X2, Y2, X1, Y1, Direction, X3, Y3, X4, Y4)),
    TakeList1)
    ;
    % Check moves for dames.
    (member(Color, [10, 20]) ->
    findall([X, Y, X1, Y1, X2, Y2, X3, Y3, X4, Y4],
    (moves_for_dames_take(X, Y, X1, Y1, X2, Y2),
    % Check if later on the piece would be able to make another move after this one.
    check_for_dames_take_again(X2, Y2, X1, Y1, X3, Y3, X4, Y4)),
    TakeList1))),
    % Recursive call.
    check_if_can_take_piece(Roots, Direction, TakeList2),
    % Compile all moves into one list.
    append(TakeList1, TakeList2, TakeList).

% Check if there are safe positions for a piece to move to.
check_safe_change_position([], _, []).
check_safe_change_position([ruut(X, Y, Color) | Roots], Direction, MoveList):-
    % Check moves for men.
    (member(Color, [1, 2]) ->
    findall([X, Y, 0, 0, X1, Y1, 0, 0, 0, 0], moves_for_men_safe_move(X, Y, Direction, X1, Y1), MoveList1)
    ;
    % Check moves for dames.
    (member(Color, [10, 20]) ->
    findall([X, Y, 0, 0, X1, Y1, 0, 0, 0, 0], moves_for_dames_safe_move(X, Y, X1, Y1), MoveList1))),
    % Recursive call.
    check_change_position(Roots, Direction, MoveList2),
    % Compile all moves into one list.
    append(MoveList1, MoveList2, MoveList).


% If there were no safe positions, find all available possitions.
check_change_position([], _, []).
check_change_position([ruut(X, Y, Color) | Roots], Direction, MoveList):-
    % Check moves for men.
    (member(Color, [1, 2]) ->
    findall([X, Y, 0, 0, X1, Y1, 0, 0, 0, 0], moves_for_men_move(X, Y, Direction, X1, Y1), MoveList1)
    ;
    % Check moves for dames.
    (member(Color, [10, 20]) ->
    findall([X, Y, 0, 0, X1, Y1, 0, 0, 0, 0], moves_for_dames_move(X, Y, X1, Y1), MoveList1))),
    % Recursive call.
    check_change_position(Roots, Direction, MoveList2),
    % Compile all moves into one list.
    append(MoveList1, MoveList2, MoveList).


% When the move is complete, check if the man became a dame.
check_dame(X, Y, Color):-
    ((Color = 1 ->
    (X = 8, retract(ruut(X, Y, Color)), assertz(ruut(X, Y, 10))))
    ;
    (Color = 2 ->
    (X = 1, retract(ruut(X, Y, Color)), assertz(ruut(X, Y, 20))))) ; true.


% Checks if there is an enemy piece in close proximity to the piece.
enemy_piece_nearby(X, Y, Color):-
    other_color(Color, EnemyColor),
    (adjacent_with_blank_space(X, Y, EnemyColor, 1, 1) ;
    adjacent_with_blank_space(X, Y, EnemyColor, 1, -1) ;
    adjacent_with_blank_space(X, Y, EnemyColor, -1, 1) ;
    adjacent_with_blank_space(X, Y, EnemyColor, -1, -1)).


% Checks if there is an enemy piece in close proximity to the piece with a blank spot across.
adjacent_with_blank_space(X, Y, EnemyColor, DirectionX, DirectionY):-
    % Enemy coords.
    X1 is X + DirectionX, Y1 is Y + DirectionY,
    within_boarder(X1), within_boarder(Y1),
    ruut(X1, Y1, EnemyColor),
    % Potentially empty spot across from the enemy.
    X2 is X - DirectionX, Y2 is Y - DirectionY,
    within_boarder(X2), within_boarder(Y2),
    ruut(X2, Y2, 0).

% ---------- MAN MOVES ----------

% Check up-left.
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X + Direction, within_boarder(X1),
    Y1 is Y - 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    dame_color(Color, _, AllyColor),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    X2 is X1 + Direction, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check up-right.
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X + Direction, within_boarder(X1),
    Y1 is Y + 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    dame_color(Color, _, AllyColor),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    X2 is X1 + Direction, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check down-left.
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X - Direction, within_boarder(X1),
    Y1 is Y - 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    dame_color(Color, _, AllyColor),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    X2 is X1 - Direction, within_boarder(X2),
    Y2 is Y1 - 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Check down-right.
moves_for_men_take(X, Y, Direction, X1, Y1, X2, Y2):-
    X1 is X - Direction, within_boarder(X1),
    Y1 is Y + 1, within_boarder(Y1),
    ruut(X, Y, Color),
    ruut(X1, Y1, OtherColor),
    dame_color(Color, _, AllyColor),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    X2 is X1 - Direction, within_boarder(X2),
    Y2 is Y1 + 1, within_boarder(Y2),
    ruut(X2, Y2, 0).

% Checks if in the next move it would be possible to take another piece.
check_for_men_take_again(X2, Y2, X1, Y1, Direction, X3, Y3, X4, Y4):-
    (moves_for_men_take(X2, Y2, Direction, X3, Y3, X4, Y4), X3 \= X1, Y3 \= Y1)
    ;
    X3 is 0, Y3 is 0, X4 is 0, Y4 is 0.

% Move a man forward to a safe position.
moves_for_men_safe_move(X, Y, Direction, X1, Y1):-
    ruut(X, Y, Color),
    X1 is X + Direction, within_boarder(X1),
    (Y1 is Y - 1 ; Y1 is Y + 1), within_boarder(Y1),
    ruut(X1, Y1, 0),
    % Check that there is no enemy piece immediately next to it with a blank space across.
    not(enemy_piece_nearby(X1, Y1, Color)).

% Simply move a man forward.
moves_for_men_move(X, Y, Direction, X1, Y1):-
    ruut(X, Y, _),
    X1 is X + Direction, within_boarder(X1),
    (Y1 is Y - 1 ; Y1 is Y + 1), within_boarder(Y1),
    ruut(X1, Y1, 0).

% ---------- DAME MOVES ----------

% Checks which way the dame moved and if there are other pieces in its way.
check_no_ally_between(X, Y, Color, X1, Y1):-
    (X > X1, Y > Y1 -> DirectionX is -1, DirectionY is -1
    ;
    X > X1, Y < Y1 -> DirectionX is -1, DirectionY is 1
    ;
    X < X1, Y > Y1 -> DirectionX is 1, DirectionY is -1
    ;
    X < X1, Y < Y1 -> DirectionX is 1, DirectionY is 1),
    check_no_ally_between_recursive(X, Y, Color, X1, Y1, DirectionX, DirectionY).

% Checks every position between the dame and its target.
check_no_ally_between_recursive(X, Y, Color, X1, Y1, DirectionX, DirectionY):-
    NextX is X + DirectionX,
    NextY is Y + DirectionY,
    (NextX = X1, NextY = Y1 -> true
    ;
    not(ruut(NextX, NextY, Color)),
    within_boarder(NextX),
    within_boarder(NextY),
    check_no_ally_between_recursive(NextX, NextY, Color, X1, Y1, DirectionX, DirectionY)).

% Check up-left.
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X + Spaces, within_boarder(X1),
    Y1 is Y - Spaces, within_boarder(Y1),
    ruut(X1, Y1, OtherColor),
    dame_color(AllyColor, _, Color),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1),
    check_no_ally_between(X, Y, OtherColor, X1, Y1),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 + AfterSpaces, within_boarder(X2),
    Y2 is Y1 - AfterSpaces, within_boarder(Y2),
    ruut(X2, Y2, 0),
    check_no_ally_between(X1, Y1, Color, X2, Y2),
    check_no_ally_between(X1, Y1, AllyColor, X2, Y2),
    check_no_ally_between(X1, Y1, OtherColor, X2, Y2).

% Check up-right.
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X + Spaces, within_boarder(X1),
    Y1 is Y + Spaces, within_boarder(Y1),
    ruut(X1, Y1, OtherColor),
    dame_color(AllyColor, _, Color),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1),
    check_no_ally_between(X, Y, OtherColor, X1, Y1),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 + AfterSpaces, within_boarder(X2),
    Y2 is Y1 + AfterSpaces, within_boarder(Y2),
    ruut(X2, Y2, 0),
    check_no_ally_between(X1, Y1, Color, X2, Y2),
    check_no_ally_between(X1, Y1, AllyColor, X2, Y2),
    check_no_ally_between(X1, Y1, OtherColor, X2, Y2).

% Check down-left.
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X - Spaces, within_boarder(X1),
    Y1 is Y - Spaces, within_boarder(Y1),
    ruut(X1, Y1, OtherColor),
    dame_color(AllyColor, _, Color),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1),
    check_no_ally_between(X, Y, OtherColor, X1, Y1),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 - AfterSpaces, within_boarder(X2),
    Y2 is Y1 - AfterSpaces, within_boarder(Y2),
    ruut(X2, Y2, 0),
    check_no_ally_between(X1, Y1, Color, X2, Y2),
    check_no_ally_between(X1, Y1, AllyColor, X2, Y2),
    check_no_ally_between(X1, Y1, OtherColor, X2, Y2).

% Check down-right.
moves_for_dames_take(X, Y, X1, Y1, X2, Y2):-
    ruut(X, Y, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    X1 is X - Spaces, within_boarder(X1),
    Y1 is Y + Spaces, within_boarder(Y1),
    ruut(X1, Y1, OtherColor),
    dame_color(AllyColor, _, Color),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1),
    check_no_ally_between(X, Y, OtherColor, X1, Y1),
    Color =\= OtherColor, OtherColor =\= AllyColor, OtherColor =\= 0,
    member(AfterSpaces, [1, 2, 3, 4, 5, 6, 7]),
    X2 is X1 - AfterSpaces, within_boarder(X2),
    Y2 is Y1 + AfterSpaces, within_boarder(Y2),
    ruut(X2, Y2, 0),
    check_no_ally_between(X1, Y1, Color, X2, Y2),
    check_no_ally_between(X1, Y1, AllyColor, X2, Y2),
    check_no_ally_between(X1, Y1, OtherColor, X2, Y2).

% Checks if in the next move it would be possible to take another piece.
check_for_dames_take_again(X2, Y2, X1, Y1, X3, Y3, X4, Y4):-
    (moves_for_dames_take(X2, Y2, X3, Y3, X4, Y4), X3 \= X1, Y3 \= Y1)
    ;
    X3 is 0, Y3 is 0, X4 is 0, Y4 is 0.

% Move a dame forward to a safe position.
moves_for_dames_safe_move(X, Y, X1, Y1):-
    ruut(X, Y, Color),
    dame_color(AllyColor, _, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    (X1 is X + Spaces ; X1 is X - Spaces), within_boarder(X1),
    (Y1 is Y + Spaces ; Y1 is Y - Spaces), within_boarder(Y1),
    % Check that there are no other pieces in the dame's way.
    ruut(X1, Y1, 0),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1),
    % Check that there is no enemy piece immediately next to it with a blank space across.
    not(enemy_piece_nearby(X1, Y1, Color)).

% Simply move a dame forward.
moves_for_dames_move(X, Y, X1, Y1):-
    ruut(X, Y, Color),
    dame_color(AllyColor, _, Color),
    member(Spaces, [1, 2, 3, 4, 5, 6, 7]),
    (X1 is X + Spaces ; X1 is X - Spaces), within_boarder(X1),
    (Y1 is Y + Spaces ; Y1 is Y - Spaces), within_boarder(Y1),
    % Check that there are no other pieces in the dame's way.
    ruut(X1, Y1, 0),
    check_no_ally_between(X, Y, Color, X1, Y1),
    check_no_ally_between(X, Y, AllyColor, X1, Y1).

% ===========================================================================
% Choose the best move
% ===========================================================================

% Chooses the best move.
%
% If Random value is 0 (false), then choose the best take move (that is, the one that
% can move again in the next iteration).
%
% If Random value is 1 (true), then that means that there are no take moves so the
% piece can move in any possible direction. The move is chosen randomly from the list.
choose_best_move(0, [], [X, Y, X1, Y1, X2, Y2, _, _, _, _], [X, Y, X1, Y1, X2, Y2]).
choose_best_move(0, [[X, Y, X1, Y1, X2, Y2, X3, Y3, X4, Y4] | TakeList],
                 [N, M, N1, M1, N2, M2, N3, M3, N4, M4], Move):-
    ruut(X, Y, Color),
    ((Color = 1 -> DameSpace is 8) ; (Color = 2 -> DameSpace is 1) ; true),
    ((N4 = 0, X4 \= 0 ; N2 = 0, X2 \= 0 ; (N2 = 0 ; N2 \= 0), X2 = DameSpace ; N = 0, X \= 0) ->
    append([], [X, Y, X1, Y1, X2, Y2, X3, Y3, X4, Y4], Best)
    ;
    append([], [N, M, N1, M1, N2, M2, N3, M3, N4, M4], Best)),
    choose_best_move(0, TakeList, Best, Move).
choose_best_move(1, MoveList, _, [X, Y, X1, Y1, X2, Y2]):-
    random_member([X, Y, X1, Y1, X2, Y2, _, _, _, _], MoveList).

% ===========================================================================
% Do the best move
% ===========================================================================

% Does all the rectracting and asserting.
do_move([X, Y, X1, Y1, X2, Y2]):-
    ruut(X, Y, Color),
    retract(ruut(X, Y, Color)),
    assertz(ruut(X, Y, 0)),
    (X1 \= 0 -> retract(ruut(X1, Y1, _)), assertz(ruut(X1, Y1, 0)) ; true),
    retract(ruut(X2, Y2, 0)),
    ((dame_color(Color, X2, DameColor) -> assertz(ruut(X2, Y2, DameColor)))
    ;
    assertz(ruut(X2, Y2, Color))).
%    assertz(ruut(X2, Y2, Color)),
%    check_dame(X2, Y2, Color).
