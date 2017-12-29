#' Day 02: I Was Told There Would Be No Math
#'
#' [I Was Told There Would Be No Math](http://adventofcode.com/2015/day/2)
#'
#' @name day02
#' @rdname day02
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
#' measure_paper_for_boxes(c("2x3x4", "1x1x10"))
#' measure_ribbon_for_boxes(c("2x3x4", "1x1x10"))
NULL

#' @rdname day02
#' @export
#' @param xs a character vector of box dimensions
measure_paper_for_boxes <- function(xs) {
  vapply(xs, measure_paper_for_single_box, 1, USE.NAMES = FALSE)
}

measure_paper_for_single_box <- function(string) {
  dims <- parse_box_dimensions(string)
  measure_box_surface_area(dims) + measure_smallest_box_side(dims)
}

#' @rdname day02
#' @export
measure_ribbon_for_boxes <- function(xs) {
  vapply(xs, measure_ribbon_for_single_box, 1, USE.NAMES = FALSE)
}

measure_ribbon_for_single_box <- function(string) {
  dims <- parse_box_dimensions(string)
  measure_box_volume(dims) + measure_smallest_box_perimeter(dims)
}

measure_box_surface_area <- function(dims) {
  l <- dims[1]
  w <- dims[2]
  h <- dims[3]
  sum(2L * c(l * w, w * h, h * l))
}

measure_box_volume <- function(dims) {
  prod(dims)
}

measure_smallest_box_side <- function(dims) {
  l <- dims[1]
  w <- dims[2]
  h <- dims[3]

  # product of two smallest edges
  prod(sort(c(l, w, h))[1:2])
}

measure_smallest_box_perimeter <- function(dims) {
  l <- dims[1]
  w <- dims[2]
  h <- dims[3]

  # visit each of the two smallest edges twice
  2L * sum(sort(c(l, w, h))[1:2])
}

parse_box_dimensions <- function(string) {
  string %>% strsplit("x") %>% unlist() %>% as.integer()
}

# A/B: Just do the math
