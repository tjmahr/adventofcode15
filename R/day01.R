#' Day 01: Not Quite Lisp
#'
#' [Not Quite Lisp](http://adventofcode.com/2015/day/1)
#'
#' @name day01
#' @rdname day01
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
#' count_parens(")())())")
#' count_parens(c("(((", "(()(()(", "))((((("))
#' count_parens_until_floor("))(((((", 1)
NULL

#' @rdname day01
#' @export
#' @param xs a vector of strings of parentheses
count_parens <- function(xs) {
  vapply(xs, count_single_parens, 1L, USE.NAMES = FALSE)
}

count_single_parens <- function(string) {
  lookup <- c("(" = 1L, ")" = -1L)
  lookup[str_tokenize(string)] %>%
    unname() %>%
    sum()
}

#' @rdname day01
#' @export
#' @param string string of parentheses to step through
#' @param target desired floor
count_parens_until_floor <- function(string, target) {
  start <- ""
  while (count_parens(start) != target) {
    if (nchar(string) == 0L) stop("Cannot find solution")
    parts <- split_after_char_n(string, 1L)
    start <- paste0(start, parts[1L])
    string <- parts[2L]
  }

  nchar(start)
}

# A: Look up integers in associative array and sum
# B: Consume characters one at a time
