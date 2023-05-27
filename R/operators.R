quantity_index <- function(f) {
  f <- match.fun(f)
  concord <- c(p1 = "q1", p0 = "q0",
               q1 = "p1", q0 = "p0",
               pb = "qb", qb = "pb",
               p  = "q",  q  = "p")

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
    stop("'f' already has an argument called 'group'")
  }

  function(..., group) {
    group <- as.factor(group)
    if (nlevels(group) == 0L) {
      stop("'group' has no levels to group by")
    }

    args <- lapply(list(...), split, group)
    res <- .mapply(f, args, ngargs)
    # same as unsplit(), but keeps names
    x <- res[[1L]][rep.int(NA_integer_, length(group))]
    split(x, group) <- res
    if (!is.null(names(x))) {
      split(names(x), group) <- lapply(res, names)
    }
    x
  }
}

balanced <- function(f, ...) {
  f <- match.fun(f)
  nbargs <- list(...)

  function(..., na.rm = FALSE) {
    dots <- list(...)
    if (na.rm) {
      dots <- lapply(dots, `[`, complete.cases(...))
    }
    do.call(f, c(dots, nbargs))
  }
}
