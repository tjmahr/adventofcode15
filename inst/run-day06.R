library(adventofcode15)
x <- readLines("./inst/input06.txt")
g <- initialize_light_grid()
g <- perform_light_command(g, x, rule_env = light_rules)
p1 <- sum(g)

g <- initialize_light_grid()
g <- perform_light_command(g, x, rule_env = light_rules_alt)
p2 <- sum(g)

stopifnot(p1 == aoc15_solutions$day06a)
stopifnot(p2 == aoc15_solutions$day06b)
