library(adventofcode15)
x <- readLines("./inst/input{{XX}}.txt")

p1 <- part_one(x)
p2 <- part_two(x)

stopifnot(p1 == aoc15_solutions$day{{XX}}a)
stopifnot(p2 == aoc15_solutions$day{{XX}}b)
