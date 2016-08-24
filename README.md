# Sudoku Solver

## What is this?:
A simple Sudoku solver, backed by a DPLL-SAT solver.

There's currently no direct capability for user input, but there's 50 test boards (in SudokuTest.hs). 
To manually solve one of these, load up SudokuTests in GHCI, and run `solve $ boardFromList <board you would like to solve>`

Running main (Main.hs) will run all the unit tests (and in the process, solve and check all the test boards).

## To build and run:
cabal run
