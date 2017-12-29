context("test-day01.R")

test_that("counting parentheses", {
  c("(())", "()()") %>%
    count_parens() %>%
    expect_equal(c(0, 0))

  c("(((", "(()(()(", "))(((((") %>%
    count_parens() %>%
    expect_equal(c(3, 3, 3))

  c("())", "))(") %>%
    count_parens() %>%
    expect_equal(c(-1, -1))

  c(")))", ")())())") %>%
    count_parens() %>%
    expect_equal(c(-3, -3))

  count_parens_until_floor(")", -1) %>% expect_equal(1)
  count_parens_until_floor("()())", -1) %>% expect_equal(5)
})
