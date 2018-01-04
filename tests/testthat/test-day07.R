context("test-day07.R")

test_that("simulating a circuit", {
  lines <- "
  NOT x -> h
  x AND y -> d
  y RSHIFT 2 -> g
  x OR y -> e
  f OR x -> j
  x LSHIFT 2 -> f
  456 -> y
  NOT y -> i
  1 AND x -> q
  100 -> if
  NOT if -> in
  in AND if -> z
  123 -> x"

  code <- lines %>%
    adventofcode15:::read_text_lines() %>%
    circuit_to_r_script()

  circuit <- source_in_circuit_env(code)
  results <- circuit_env_to_list(circuit)

  testthat::expect_equal(results$d, 72)
  testthat::expect_equal(results$e, 507)
  testthat::expect_equal(results$f, 492)
  testthat::expect_equal(results$g, 114)
  testthat::expect_equal(results$h, 65412)
  testthat::expect_equal(results$i, 65079)
  testthat::expect_equal(results$x, 123)
  testthat::expect_equal(results$y, 456)
  testthat::expect_equal(results$q, 1)
})
