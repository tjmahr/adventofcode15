split_after_char_n <- function(string, n) {
  to_take <- min(n, nchar(string))
  x1 <- substr(string, 1L, to_take)
  x2 <- substr(string, to_take + 1L, nchar(string))
  c(x1, x2)
}

str_tokenize <- function(x) {
  unlist(strsplit(x, ""))
}

int_to_n_bits <- function(xs, n = 16, start = 1) {
  indices <- seq_len(length(xs))
  # intToBits(xs) returns 32 bits for each number of xs and each bit has a
  # leading zero like 01 00 00 00. The item on the left is the smallest digit.
  #
  # as.integer() will remove the leading zeros.
  #
  # The rep() lines produces 1 repeated 32 times followed by 2 repeated 32
  # times, etc. Splitting on these values will get us a list where each element
  # in the list are the  binary digits for each number.
  bits <- split(as.integer(intToBits(xs)), rep(indices, each = 32))

  # Here we convert the vectors of binary digits into strings. Using `n:start`
  # will reverse the digits so the smallest digit is on the right.
  vapply(bits, function(x) paste0(x[n:start], collapse = ""),
         character(1), USE.NAMES = FALSE)

}

read_text_lines <- function(x) {
  x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    keep_if(function(x) x != "")
}

keep_if <- function(data, predicate) {
  Filter(predicate, data)
}
