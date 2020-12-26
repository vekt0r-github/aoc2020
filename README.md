# aoc2020 in Haskell
Solutions to days 12-25 of [Advent of Code 2020](https://adventofcode.com/2020) in Haskell, written by someone who had never tried functional programming before day 12.

## Usage
A compiled executable covering all code can be run with `app/aoc2020.exe [<day_number> ..]`; it must be run from the repository's base folder. The source code is located in src/ (day11.py is thrown in for fun because i like the solution), and the input files are in input/.

## Notes
16: rewrote findPerm to reuse in day 21
18: the eval function in Language.Haskell.Interpreter, makes part 2 a lot easier, but part 1 is just as annoying

slowest running time awards:
1. day 19 (120s)
2. day 15 (55s)
3. day 23 (45s)
4. others (<3s)
