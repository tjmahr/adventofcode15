split_after_char_n <- function(string, n) {
  to_take <- min(n, nchar(string))
  x1 <- substr(string, 1L, to_take)
  x2 <- substr(string, to_take + 1L, nchar(string))
  c(x1, x2)
}

str_tokenize <- function(x) {
  unlist(strsplit(x, ""))
}
