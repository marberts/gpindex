quantity_index <- function(f) {
  f <- match.fun(f)
  concord <- c(p1 = "q1", p0 = "q0", 
               q1 = "p1", q0 = "p0", 
               pb = "qb", qb = "pb",
               p  = "q",  q  = "p")
  # return function
  function(...) {
    dots <- list(...)
    pqs <- names(dots) %in% concord
    names(dots)[pqs] <- concord[names(dots)[pqs]]
    do.call(f, dots)
  }
}

grouped <- function(f, ...) {
  f <- match.fun(f)
  ngargs <- list(...)
  if ("group" %in% names(formals(f))) {
    stop(gettext("'f' already has an argument called 'group'"))
  }
  # return function
  function(..., group) {
    group <- as.factor(group)
    args <- lapply(list(...), split, group)
    res <- .mapply(f, args, ngargs)
    res <- unsplit(res, group)
    attributes(res) <- NULL # unsplit mangles attributes
    res
  }
}

balanced <- function(f, ...) {
  f <- match.fun(f)
  nbargs <- list(...)
  # return function
  function(..., na.rm = FALSE) {
    dots <- list(...)
    if (na.rm) {
      dots <- lapply(dots, `[`, complete.cases(...))
    }
    do.call(f, c(dots, nbargs))
  }
}
