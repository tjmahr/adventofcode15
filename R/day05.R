#' Day 05: Doesn't He Have Intern-Elves For This?
#'
#' [Doesn't He Have Intern-Elves For This?](http://adventofcode.com/2015/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#'
#'
#' **Part Two**
#'
#'
#'
#' @examples
#' strings <- c("ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp",
#'              "haegwjzuvuyypxyu", "dvszwmarrgswjxmb")
#' rule_list <- list(contains_three_vowels, contains_a_repeated_letter,
#'                   lacks_illegal_strings)
#'
#' check_for_nice_words(strings, rule_list)
NULL

#' @rdname day05
#' @export
check_for_nice_words <- function(strings, rule_list) {
  f <- Reduce(and, rule_list)
  f(strings)
}

# Combine two predicates
and <- function(f1, f2) {
  function(x) f1(x) & f2(x)
}


#' @rdname day05
#' @export
contains_three_vowels <- function(xs) {
  f_one <- function(string) {
    sum(str_tokenize(string) %in% c("a", "e", "i", "o", "u")) >= 3
  }
  vapply(xs, f_one, TRUE, USE.NAMES = FALSE)
}

#' @rdname day05
#' @export
contains_a_repeated_letter <- function(xs) {
  f_one <- function(string) {
    tokens <- str_tokenize(string)
    any(c(tokens[-length(tokens)]) == c(tokens[-1L]))
  }
  vapply(xs, f_one, TRUE, USE.NAMES = FALSE)
}

#' @rdname day05
#' @export
#' @param blacklist a vector of illegal sequences
lacks_illegal_strings <- function(xs, blacklist = c("ab", "cd", "pq", "xy")) {
  f_one <- function(string) {
    tokens <- str_tokenize(string)
    pairs <- paste0(tokens[-length(tokens)], c(tokens[-1L]))
    !any(pairs %in% blacklist)
  }
  vapply(xs, f_one, TRUE, USE.NAMES = FALSE)
}

#' @rdname day05
#' @export
contains_repeated_pair <- function(xs) {
  f_one <- function(string) {
    tokens <- str_tokenize(string)
    pairs <- paste0(tokens[-length(tokens)], c(tokens[-1L]))
    repeats <- names(table(pairs)[table(pairs) >= 2])
    success <- FALSE
    for (repeated in repeats) {
      matches <- which(pairs == repeated)
      good_pair <- any(max(matches) - min(matches) > 1)
      success <- success || good_pair
    }
    success
  }
  vapply(xs, f_one, TRUE, USE.NAMES = FALSE)
}

#' @rdname day05
#' @export
contains_staggered_repeat <- function(xs) {
  f_one <- function(string) {
    tokens <- str_tokenize(string)
    any(head(tokens, -2) == tail(tokens, -2))
  }
  vapply(xs, f_one, TRUE, USE.NAMES = FALSE)
}

# A/B: Functional programming
