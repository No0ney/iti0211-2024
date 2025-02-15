This repository contains my studies of Prolog, a logic programming language used primarily in artificial intelligence (AI), natural language processing (NLP), and expert systems.

The `KABE` folder contains a program I wrote that can play checkers with other programs or a human. To run it, SWI-Prolog (https://www.swi-prolog.org/) must be installed first. After it has been installed, the SWI-Prolog application should appear on the machine, and after opening it, input the next commands:
```
?- consult('path/to/kabe.pl').
?- consult('path/to/graphicArbiter.pl').
```

After these steps, you have several options to play:

1. Run `t(h)` to play against the program yourself;
2. Run `t(t)` to play against the program yourself and make the program wait for 0.5 seconds before its turn;
3. Run `t(0.5)` to play against the program yourself and make the program wait for however long you choose before its turn (0.5 is half a second, 2 is 2 seconds, etc.).

When playing against the program yourself, to make a move you need to click and drag the piece you wish to move. If you make an "illegal" move (that is, the move that cannot be made due to the rules of checkers or there was an option to take the enemy's piece but it wasn't done) then the opponent automatically wins the game. If after 15 turns no piece was taken, the game ends with a tie. If you can take multiple enemy pieces in a row, your turn won't end until all possible moves are done.

If you wish to make the program play against itself, then in the `graphicArbiter.pl` file the 11th line should be changed to:
```
valged(larry).
```

After changing the contents of the `graphicArbiter.pl` file, the `consult('path/to/graphicArbiter.pl').` command needs to be re-entered to apply the changes. Afterwards, there are some more options to play:

1. Run `t(s)` to play in step mode, where you can observe the game step-by-step by pressing the spacebar (or other buttons). If you press the `c` button, the game will continue automatically without the need to press any buttons;
2. Run `t(t)` to make the programs pause for 0.5 seconds before their turns;
3. Run `t(0.5)` to make the programs pause for however long you choose before their turns (0.5 is half a second, 2 is 2 seconds, etc.).

To close the SWI-Prolog application you can use the command `halt.`.

The `graphicArbiter.pl` file contains these instructions in Estonian.
