library(adventofcode15)
x <- readLines("./inst/input03.txt")

p1 <- trace_path(x)
p2 <- trace_both_paths(x)

stopifnot(length(unique(p1)) == aoc15_solutions$day03a)
stopifnot(length(unique(p2)) == aoc15_solutions$day03b)
