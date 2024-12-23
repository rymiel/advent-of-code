# Advent of code

Each year is a separate directory.

## Crystal

Years 2015, 2016, 2019, and 2023 are solved in Crystal. These directories are their own separate project each.
Any dependencies, if any, are stored in the year's directory. Solutions can be run using `crystal run`, passing the path
to the solution file while being in the year's directory.

## Ocaml

Years 2020 and 2024 are solved in Ocaml. There is a global dune project setup to facililtate the use of the shared utilities library `aoclib`.
Solutions can be run using `dune exec aoc2024 day1 a`, changing the year number or day number as appropriate.
