#' Day 06: Probably a Fire Hazard
#'
#' [Probably a Fire Hazard](http://adventofcode.com/2015/day/6)
#'
#' @name day06
#' @rdname day06
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
#' grid <- initialize_light_grid(7, 5, 0)
#' grid
#'
#' grid <- perform_light_command(grid, "turn on 0,0 through 4,4")
#' grid
#'
#' commands <- c("turn off 0,1 through 4,1", "turn off 0,3 through 4,3")
#' grid <- perform_light_command(grid, commands)
#' grid
#'
#' grid <- perform_light_command(grid, "toggle 0,0 through 6,4")
#' grid
NULL

#' @rdname day06
#' @export
#' @param rows,cols dimensions of light grid
#' @param start initalize value for lights
initialize_light_grid <- function(rows = 1000L, cols = 1000L, start = 0L) {
  matrix(rep(start, rows * cols), ncol = cols)
}

#' @rdname day06
#' @export
#' @param grid a grid of lights
#' @param command_text a vector with lighting commands
#' @param rule_env environment containing definitions for `turn_on_lights()`,
#'   `turn_off_lights()` and `toggle_lights()`. Defaults to `light_rules` which
#'   are the definitions for Part 1. `light_rules_alt` are the definitions for
#'   Part 2.
perform_light_command <- function(grid, command_text, rule_env = light_rules) {
  for (line in command_text) {
    parsed <- parse_light_instruction(line)
    grid <- rlang::invoke(parsed$f, parsed[-1L], grid = grid, .env = rule_env)
  }
  grid
}

get_rectangle_indices <- function(c1, c2) {
  # Add 1 to change to 1-based indices
  x <- seq(c1[1], c2[1]) + 1
  y <- seq(c1[2], c2[2]) + 1
  indices <- expand.grid(x, y)
  cbind(indices[, 1], indices[, 2])
}

parse_light_instruction <- function(x) {
  cs <- as.numeric(unlist(stringr::str_extract_all(x, "\\d+")))
  f <- if (stringr::str_detect(x, "turn on")) {
    "turn_on_lights"
  } else if (stringr::str_detect(x, "turn off")) {
    "turn_off_lights"
  } else {
    "toggle_lights"
  }
  list(f = f, c1 = cs[1L:2L], c2 = cs[3L:4L])
}

#' @rdname day06
#' @export
light_rules <- rlang::env(
  turn_on_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- 1L
    grid
  },

  turn_off_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- 0L
    grid
  },

  toggle_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- ifelse(grid[indices], 0L, 1L)
    grid
  }
)

#' @rdname day06
#' @export
light_rules_alt <- rlang::env(
  turn_on_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- grid[indices] + 1L
    grid
  },

  turn_off_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- grid[indices] - 1L
    grid[indices] <- ifelse(grid[indices] < 0L, 0L, grid[indices])
    grid
  },

  toggle_lights = function(grid, c1, c2) {
    indices <- get_rectangle_indices(c1, c2)
    grid[indices] <- grid[indices] + 2L
    grid
  }
)

# A: Parse text and invoke. Use m[cbind()] trick to select elements.
# B: Make the environment containing the function definitions an
# argument/option.
