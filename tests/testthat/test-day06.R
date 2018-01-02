context("test-day06.R")

test_that("manipulating a grid of lights", {
  g0 <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                  .Dim = c(7L, 5L))
  g1 <- structure(c(1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1,
                    1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0),
                  .Dim = c(7L, 5L))
  g2 <- structure(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
                    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0),
                  .Dim = c(7L, 5L))
  g3 <- structure(c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0,
                    0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1),
                  .Dim = c(7L, 5L))

  initialize_light_grid(7, 5, 0) %>%
    expect_equal(g0) %>%
    perform_light_command("turn on 0,0 through 4,4") %>%
    expect_equal(g1) %>%
    perform_light_command(c("turn off 0,1 through 4,1",
                            "turn off 0,3 through 4,3")) %>%
    expect_equal(g2) %>%
    perform_light_command("toggle 0,0 through 6,4") %>%
    expect_equal(g3)

  # Alternative rules
  g0 <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                  .Dim = c(7L, 5L))
  g1 <- structure(c(1, 2, 2, 1, 1, 0, 0, 1, 2, 2, 1, 1, 0, 0, 1, 2, 2,
                    1, 1, 0, 0, 1, 2, 2, 1, 1, 0, 0, 1, 2, 2, 1, 1, 0, 0),
                  .Dim = c(7L, 5L))

  initialize_light_grid(7, 5, 0) %>%
    expect_equal(g0) %>%
    perform_light_command("turn off 0,0 through 4,4", light_rules_alt) %>%
    expect_equal(g0) %>%
    perform_light_command("turn on 0,0 through 4,4", light_rules_alt) %>%
    perform_light_command("turn on 1,0 through 2,4", light_rules_alt) %>%
    expect_equal(g1) %>%
    perform_light_command(c("toggle 0,0 through 6,4",
                            "toggle 0,0 through 6,4"), light_rules_alt) %>%
    expect_equal(g1 + 4)
})
