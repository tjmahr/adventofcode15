library(adventofcode15)
x <- readLines("./inst/input02.txt")

p1 <- measure_paper_for_boxes(x) %>% sum()
p2 <- measure_ribbon_for_boxes(x) %>% sum()

stopifnot(p1 == aoc15_solutions$day02a)
stopifnot(p2 == aoc15_solutions$day02b)
