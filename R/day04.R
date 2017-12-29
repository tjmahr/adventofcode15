#' Day 04: The Ideal Stocking Stuffer
#'
#' [The Ideal Stocking Stuffer](http://adventofcode.com/2015/day/4)
#'
#' @name day04
#' @rdname day04
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
#' mine_advent_coins("abcdef", 600000)
NULL

#' @rdname day04
#' @export
#' @param seed seed for the hashing algorithm
#' @param start number to start at
#' @param criterion rule to judge hashes
mine_advent_coins <- function(seed, start = 1L, criterion = earns_advent_coin) {
  while (!criterion(paste0(seed, start))) {
    if (start %% 100000L == 0L) message(start)
    start <- start + 1L
  }
  start
}

#' @rdname day04
#' @export
#' @param x string to check
earns_advent_coin <- function(x) {
  has_five_leading_zeroes(digest::digest(x, "md5", serialize = FALSE))
}
#' @rdname day04
#' @export
earns_super_advent_coin <- function(x) {
  has_six_leading_zeroes(digest::digest(x, "md5", serialize = FALSE))
}

has_five_leading_zeroes <- function(x) substr(x, 1, 5) == "00000"
has_six_leading_zeroes <- function(x) substr(x, 1, 6) == "000000"

# A/B: Make the coin criterion a function and pass it in.
