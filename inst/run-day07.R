library(adventofcode15)
x <- readLines("./inst/input07.txt")

# Peel off 200 layers of nesting
for (i in 1:200) {
  x <- evaluate_circuit_functions(inline_circuit_value(x))
}
x

circuit_code <- circuit_to_r_script(x)
circuit_env <- source_in_circuit_env(circuit_code)



x <- readLines("./inst/input07.txt")
b_setter <- stringr::str_which(x, "-> b$")
x[b_setter] <- "956 -> b"

# Peel off 200 layers of nesting
for (i in 1:200) {
  x <- evaluate_circuit_functions(inline_circuit_value(x))
}

circuit_code2 <- circuit_to_r_script(x)
circuit_env2 <- source_in_circuit_env(circuit_code2)

stopifnot(circuit_env$a() == aoc15_solutions$day07a)
stopifnot(circuit_env2$a() == aoc15_solutions$day07b)

