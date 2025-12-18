# Advent of code

This repo contains my solutions to [Advent of Code](https://adventofcode.com/) and [MITS advent calendar](https://kood.mits.ee/). Each year is a separate directory.

## Crystal

Years 2015, 2016, 2019, and 2023 are solved in Crystal. These directories are their own separate project each.
Any dependencies, if any, are stored in the year's directory. Solutions can be run using `crystal run`, passing the path
to the solution file while being in the year's directory.

## Ocaml

Years 2020, 2021, 2022 and 2024 are solved in Ocaml. There is a global dune project setup to facililtate the use of the shared utilities library `aoclib`.
Solutions can be run using `dune exec aoc2024 day1 a`, changing the year number or day number as appropriate.

## C++

Year 2025 is solved using C++23. There is a global Makefile for these solutions and a shared utilities library `aoclib++`.
Solutions can be run using `make 2025/day1.a`, changing the year number or day number as appropriate.
