context("test-day04.R")

test_that("hashing works", {
  steps <- mine_advent_coins("abcdef", 600000)
  expect_equal(steps, 609043)
})
