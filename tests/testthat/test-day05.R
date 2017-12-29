context("test-day05.R")

test_that("checking several strings", {
  strings <- c("ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp",
               "haegwjzuvuyypxyu", "dvszwmarrgswjxmb")
  rule_list <- list(contains_three_vowels, contains_a_repeated_letter,
                    lacks_illegal_strings)

  check_for_nice_words(strings, rule_list) %>%
    expect_equal(c(TRUE, TRUE, FALSE, FALSE, FALSE))

  contains_repeated_pair(c("xyxy", "aabcdefgaa", "aaa")) %>%
    expect_equal(c(TRUE, TRUE, FALSE))

  contains_repeated_pair("xxxx") %>% expect_true()

  contains_staggered_repeat(c("xyx", "abcdefeghi", "aaa")) %>%
    expect_equal(c(TRUE, TRUE, TRUE))

  rule_list2 <- list(contains_repeated_pair, contains_staggered_repeat)

  strings <- c("qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg",
               "ieodomkazucvgmuy")
  check_for_nice_words(strings, rule_list2) %>%
    expect_equal(c(TRUE, TRUE, FALSE, FALSE))


})
