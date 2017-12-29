library(adventofcode15)
x <- readLines("./inst/input04.txt")

p1 <- mine_advent_coins(x, 100100L, earns_advent_coin)
p2 <- mine_advent_coins(x, 3938000L, earns_super_advent_coin)

stopifnot(p1 == aoc15_solutions$day04a)
stopifnot(p2 == aoc15_solutions$day04b)
