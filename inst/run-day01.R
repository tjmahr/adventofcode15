library(adventofcode15)
x <- readLines("./inst/input01.txt")

p1 <- count_parens(x)
p2 <- count_parens_until_floor(x, -1L)

stopifnot(p1 == aoc15_solutions$day01a)
stopifnot(p2 == aoc15_solutions$day01b)
