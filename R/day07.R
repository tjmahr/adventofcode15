#' Day 07: Some Assembly Required
#'
#' [Some Assembly Required](http://adventofcode.com/2015/day/7)
#'
#' @name day07
#' @rdname day07
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
#' lines <- "
#'   NOT x -> h
#' x AND y -> d
#' y RSHIFT 2 -> g
#' x OR y -> e
#' f OR x -> j
#' x LSHIFT 2 -> f
#' 456 -> y
#' NOT y -> i
#' 1 AND x -> q
#' 100 -> if
#' NOT if -> in
#' in AND if -> z
#' 123 -> x"
#'
#' code <- lines %>%
#'   adventofcode15:::read_text_lines() %>%
#'   circuit_to_r_script()
#'
#' circuit <- source_in_circuit_env(code)
#' circuit_env_to_list(circuit)
#'
#' lines %>%
#'   adventofcode15:::read_text_lines() %>%
#'   inline_circuit_value() %>%
#'   evaluate_circuit_functions() %>%
#'   inline_circuit_value()
NULL


# Problem is that input lines arrive out of order, so treat everything as a
# function so that lazy evaluation will use values on demand.

#' @rdname day07
#' @export
#' @param xs character vector of circuit wirses
circuit_to_r_script <- function(xs) {
  vapply(xs, circuit_to_r, "text", USE.NAMES = FALSE)
}

#' @rdname day07
#' @export
#' @param x a string with a circuit wire description
circuit_to_r <- function(x) {
  parts <- unlist(strsplit(x, " -> "))
  name <- circuit_escape(parts[[2]])
  paste0(name, " <- ", circuit_to_r_body(x))
}

circuit_to_r_body <- function(x) {
  parts <- unlist(strsplit(x, " -> "))
  body <- parts[[1]] %>%
    circuit_escape() %>%
    stringr::str_replace("(.+) (AND) (.+)", "circuit_and(\\1, \\3)") %>%
    stringr::str_replace("(.+) (OR) (.+)", "circuit_or(\\1, \\3)") %>%
    stringr::str_replace("(.+) (LSHIFT) (.+)", "circuit_lshift(\\1, \\3)") %>%
    stringr::str_replace("(.+) (RSHIFT) (.+)", "circuit_rshift(\\1, \\3)") %>%
    stringr::str_replace("(NOT) (.+)", "circuit_not(\\2)")

  if (!stringr::str_detect(body, "circuit")) {
    if (!rlang::is_syntactic_literal(rlang::parse_expr(body))) {
      body <- paste0(body, "()")
    }

    body <- paste0("function() {", body, "}")
  }
  body
}

circuit_escape <- function(xs) {
  # Work around for when circuit name is `if` or `in`
  xs <- stringr::str_replace_all(xs, "(\\W|^)if(\\W|$)", "\\1if_\\2")
  xs <- stringr::str_replace_all(xs, "(\\W|^)in(\\W|$)", "\\1in_\\2")
  xs
}


#' @rdname day07
#' @export
#' @param code lines of R code describing a circuit
source_in_circuit_env <- function(code) {
  f <- tempfile()
  writeLines(code, f)
  where <- new.env(parent = parent.frame())
  source(f, local = where)
  where
}

#' @rdname day07
#' @export
#' @param env an environment containing circuit definitions
circuit_env_to_list <- function(env) {
  values <- list()
  for (fn in ls(env)) {
    values[[fn]] <- rlang::invoke(fn, .env = env, .bury = NULL)
  }
  values
}

#' @rdname day07
#' @export
#' @param f1,f2 funcitons to evaluate for their values
circuit_and <- function(f1, f2) {
  function() {
    g1 <- if (is.numeric(f1)) f1 else f1()
    g2 <- if (is.numeric(f2)) f2 else f2()
    c_and(g1, g2)
  }
}

#' @rdname day07
#' @export
circuit_or <- function(f1, f2) {
  function() {
    g1 <- if (is.numeric(f1)) f1 else f1()
    g2 <- if (is.numeric(f2)) f2 else f2()
    c_or(g1, g2)
  }
}

#' @rdname day07
#' @export
circuit_not <- function(f1) {
  function() {
    g1 <- if (is.numeric(f1)) f1 else f1()
    c_not(g1)
  }
}

#' @rdname day07
#' @export
#' @param n number of bits to shift
circuit_lshift <- function(f1, n) {
  function() {
    g1 <- if (is.numeric(f1)) f1 else f1()
    c_lshift(g1, n)
  }
}

#' @rdname day07
#' @export
circuit_rshift <- function(f1, n) {
  function() {
    g1 <- if (is.numeric(f1)) f1 else f1()
    c_rshift(g1, n)
  }
}

c_and <- function(int1, int2) bitwAnd(int1, int2)

c_or <- function(int1, int2) bitwOr(int1, int2)

c_not <- function(int) {
  strtoi(int_to_n_bits(bitwNot(int), 16), base = 2)
}

c_lshift <- function(int, n) {
  strtoi(int_to_n_bits(bitwShiftL(int, n), 16), base = 2)
}

c_rshift <- function(int, n) {
  strtoi(int_to_n_bits(bitwShiftR(int, n), 16), base = 2)
}



# My second attempt because the first way was being too slow. I am using what I
# already wrote above although a cleaner solution would be to start all over and
# not build on top of the everything is a function approach from above. This
# approach is to inline values into the text to reduce the depth of the function
# calls.

# xs <- read_text_lines(lines)
# xs <- inline_circuit_value(xs)

#' @rdname day07
#' @export
inline_circuit_value <- function(xs) {
  assignment_lines <- stringr::str_which(xs, "^\\d+ ->")
  assignments <- xs[assignment_lines]
  for (line in assignments) {
    parts <- unlist(strsplit(line, " -> "))

    detect <- sprintf("(\\W|^)%s(\\W)", parts[2])
    replace <- sprintf("\\1%s\\2", parts[1])
    xs <- stringr::str_replace_all(xs, detect, replace)
  }
  xs[-assignment_lines]
}

#' @rdname day07
#' @export
evaluate_circuit_functions <- function(xs) {
  for (x_i in seq_along(xs)) {
    x <- xs[x_i]
    if (is_just_number(x)) next

    if (can_evaluate_circuit(x)) {
      direct <- stringr::str_replace(circuit_to_r_body(x), "circuit", "c")
      command <- rlang::parse_expr(direct)
      value <- rlang::eval_tidy(command)
      parts <- unlist(strsplit(x, " -> "))
      parts[1] <- value
      xs[x_i] <- paste0(parts[1], " -> ", parts[2])
    }
  }
  xs
}

can_evaluate_circuit <- function(x) {
  parts <- unlist(strsplit(x, " -> "))

  has_letter <- parts[[1]] %>%
    stringr::str_replace_all("( |OR|NOT|AND|OR|RSHIFT|LSHIFT)", "") %>%
    stringr::str_detect("[A-z]")
  !has_letter
}

is_just_number <- function(x) {
  parts <- unlist(strsplit(x, " -> "))

  has_nondigit <- stringr::str_detect(parts[[1]], "\\D")
  !has_nondigit
}


# A/B: Nonstandard evaluation, delayed evaluation using functions, simplifing
# code by evaluating/inlining values
