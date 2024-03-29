% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day07.R
\name{day07}
\alias{day07}
\alias{circuit_to_r_script}
\alias{circuit_to_r}
\alias{source_in_circuit_env}
\alias{circuit_env_to_list}
\alias{circuit_and}
\alias{circuit_or}
\alias{circuit_not}
\alias{circuit_lshift}
\alias{circuit_rshift}
\alias{inline_circuit_value}
\alias{evaluate_circuit_functions}
\title{Day 07: Some Assembly Required}
\usage{
circuit_to_r_script(xs)

circuit_to_r(x)

source_in_circuit_env(code)

circuit_env_to_list(env)

circuit_and(f1, f2)

circuit_or(f1, f2)

circuit_not(f1)

circuit_lshift(f1, n)

circuit_rshift(f1, n)

inline_circuit_value(xs)

evaluate_circuit_functions(xs)
}
\arguments{
\item{xs}{character vector of circuit wirses}

\item{x}{a string with a circuit wire description}

\item{code}{lines of R code describing a circuit}

\item{env}{an environment containing circuit definitions}

\item{f1, f2}{funcitons to evaluate for their values}

\item{n}{number of bits to shift}
}
\description{
\href{http://adventofcode.com/2015/day/7}{Some Assembly Required}
}
\details{
\strong{Part One}

\strong{Part Two}
}
\examples{
lines <- "
  NOT x -> h
x AND y -> d
y RSHIFT 2 -> g
x OR y -> e
f OR x -> j
x LSHIFT 2 -> f
456 -> y
NOT y -> i
1 AND x -> q
100 -> if
NOT if -> in
in AND if -> z
123 -> x"

code <- lines \%>\%
  adventofcode15:::read_text_lines() \%>\%
  circuit_to_r_script()

circuit <- source_in_circuit_env(code)
circuit_env_to_list(circuit)

lines \%>\%
  adventofcode15:::read_text_lines() \%>\%
  inline_circuit_value() \%>\%
  evaluate_circuit_functions() \%>\%
  inline_circuit_value()
}
