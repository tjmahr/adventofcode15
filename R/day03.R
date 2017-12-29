#' Day 03: Perfectly Spherical Houses in a Vacuum
#'
#' [Perfectly Spherical Houses in a Vacuum](http://adventofcode.com/2015/day/3)
#'
#' @name day03
#' @rdname day03
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
#' trace_path("^v^v^v^v^v")
#' trace_both_paths("^v^v^v^v^v")
NULL

#' @rdname day03
#' @export
#' @param string a string of steps
trace_path <- function(string) {
  # Map each step to a change in position
  steps <- str_tokenize(string)
  mapping <- matrix(c(
     0L,  1L,
     0L, -1L,
     1L,  0L,
    -1L, -0L), ncol = 2, byrow = TRUE)

  colnames(mapping) <- c("x", "y")
  rownames(mapping) <- c("^", "v", ">", "<")
  changes <- mapping[steps, , drop = FALSE]

  # Compute rolling sum to get a list of positions
  changes <- split(changes, seq_len(nrow(changes)))
  Reduce(`+`, changes, init = c(0, 0), accumulate = TRUE)
}

#' @rdname day03
#' @export
trace_both_paths <- function(string) {
  steps <- str_tokenize(string)
  even <- steps[seq_along(steps) %% 2 == 0] %>% paste0(collapse = "")
  odd <- steps[seq_along(steps) %% 2 != 0] %>% paste0(collapse = "")
  c(trace_path(even), trace_path(odd))
}

# A: Map symbols onto steps, Reduce(+, steps, c(0, 0))
# B: Split apply combine
