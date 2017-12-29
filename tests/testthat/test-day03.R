context("test-day03.R")

test_that("tracing steps", {
  trace_path(">") %>% unique() %>% length() %>% expect_equal(2)
  trace_path("^>v<") %>% unique() %>% length() %>% expect_equal(4)
  trace_path("^v^v^v^v^v") %>% unique() %>% length() %>% expect_equal(2)

  trace_both_paths("^v") %>% unique() %>% length() %>% expect_equal(3)
  trace_both_paths("^>v<") %>% unique() %>% length() %>% expect_equal(3)
  trace_both_paths("^v^v^v^v^v") %>% unique() %>% length() %>% expect_equal(11)
})
