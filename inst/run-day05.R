library(adventofcode15)
x <- readLines("./inst/input05.txt")

rule_list1 <- list(contains_three_vowels, contains_a_repeated_letter,
                   lacks_illegal_strings)
rule_list2 <- list(contains_repeated_pair, contains_staggered_repeat)

checks1 <- check_for_nice_words(x, rule_list1)
checks2 <- check_for_nice_words(x, rule_list2)

p1 <- sum(checks1)
p2 <- sum(checks2)

stopifnot(p1 == aoc15_solutions$day05a)
stopifnot(p2 == aoc15_solutions$day05b)
