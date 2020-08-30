length1 <- function(x, mode) {
  length(x) == 1L && is.vector(x, mode) && !is.na(x)
}

same_length <- function(...) {
  res <- vapply(list(...), length, numeric(1))
  all(res == res[1])
}

is_numeric <- function(...) {
  res <- vapply(list(...), is.vector, logical(1), mode = "numeric")
  all(res)
}