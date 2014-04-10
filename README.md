Battleship Haskell
==================

This project was created for the course Programming Paradigms at the University of Antwerp, 2014.

Running the game
----------------

To run the game one simply has to run `Battleship.main`. This can be done easiest by running `runhaskell Battleship.hs`.

The game interactively prompts for input. First names will be asked, then ship coordinates and afterwards the game can be
played by interactively entering coordinates to target. Feedback will be provided about the effectiveness of the shot.

Here is an example game where An utterly destroys Bert while Bert tries to hit An in the same spot over and over again.
It is of course a happy day scenario with no faulty input.

```
An
Bert
(0,0);(0,1)
(6,7);(7,7);(8,7)
(3,3);(3,4);(3,5);(3,6)
(5,9);(6,9);(7,9);(8,9);(9,9)
(6,6);(6,7)
(3,2);(3,3);(3,4)
(9,6);(9,7);(9,8);(9,9)
(4,9);(5,9);(6,9);(7,9);(8,9)
(6,6)
(6,7)
(3,2)
(3,3)
(6,7)
(6,7)
(6,7)
(3,4)
(9,6)
(9,7)
(9,8)
(6,7)
(6,7)
(9,9)
(4,9)
(5,9)
(6,9)
(6,7)
(6,7)
(7,9)
(8,9)
```

It can also be run using the following command.

```
cat example.txt | runhaskell Battleship.hs
```

Example output.

```
Who's Player 1
Who's Player 2
An's ships
Bert's ships
It's An's turn. They get 4 shots.
An hit Bert at (6,6)!
An hit Bert at (6,7)!
You sunk my battleship!
An hit Bert at (3,2)!
An hit Bert at (3,3)!
It's Bert's turn. They get 3 shots.
Bert hit An at (6,7)!
Bert Misses.
Bert Misses.
It's An's turn. They get 4 shots.
An hit Bert at (3,4)!
You sunk my battleship!
An hit Bert at (9,6)!
An hit Bert at (9,7)!
An hit Bert at (9,8)!
It's Bert's turn. They get 2 shots.
Bert Misses.
Bert Misses.
It's An's turn. They get 4 shots.
An hit Bert at (9,9)!
You sunk my battleship!
An hit Bert at (4,9)!
An hit Bert at (5,9)!
An hit Bert at (6,9)!
It's Bert's turn. They get 1 shots.
Bert Misses.
It's An's turn. They get 4 shots.
An Misses.
An hit Bert at (7,9)!
An hit Bert at (8,9)!
You sunk my battleship!
Bert lost.
===========
=  Recap  =
===========
Bert's field:
..........
..........
...x......
...x......
...x......
..........
......x..x
......x..x
.........x
....xxxxxx
An's field:
o.........
o.........
..........
...o......
...o......
...o......
...o......
......xoo.
..........
.....ooooo
```

About the implementation
------------------------

Some basic error mechanisms are in place. Faulty input is detected and some IO errors are handled. Validity of a ship's
location is checked as well: both whether it overlaps with others or whether it lies in a continuous straight line.

When the game's finished, both resulting playing boards will be showed with an ASCII representation.

The program is written as Haskell-y as a strongly IO-oriented program allows. Since it's still a highly interactive game,
this wasn't as clean as I'd have wanted, since Haskell is rather poor at handling sequential things. However, I tried
keeping the impure and pure parts as separate as possible. On the surface there are some monolithic input gatherers
that call pure and clean Haskell functions below. These can be detected by their `IO a` return types.