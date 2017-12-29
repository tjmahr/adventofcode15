context("test-day02.R")

test_that("measuring wrapping paper for boxes", {
  c("2x3x4", "1x1x10") %>%
    measure_paper_for_boxes() %>%
    expect_equal(c(58, 43))

  c("2x3x4", "1x1x10") %>%
    measure_ribbon_for_boxes() %>%
    expect_equal(c(34, 14))
})
