#' Quantity index operator
#'
#' Remaps price arguments into quantity argument (and vice versa) to turn a
#' price index into a quantity index.
#'
#' @param f A [price-index function][price_indexes].
#'
#' @returns
#' A function like `f`, except that the role of prices/quantities is reversed.
#'
#' @examples
#' p1 <- price6[[3]]
#' p0 <- price6[[2]]
#' q1 <- quantity6[[3]]
#' q0 <- quantity6[[2]]
#'
#' # Remap argument names to be quantities rather than prices.
#'
#' quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)
#'
#' laspeyres_index(p1 = q1, p0 = q0, q0 = p0)
#'
#' # Works with the index_weights() functions, too.
#'
#' quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
#'
#' @family operators
#' @export
quantity_index <- function(f) {
  f <- match.fun(f)
  concord <- c(
    p1 = "q1", p0 = "q0",
    q1 = "p1", q0 = "p0",
    pb = "qb", qb = "pb",
    p = "q", q = "p"
  )

  function(...) {
    dots <- list(...)
    pqs <- names(dots) %in% concord
    names(dots)[pqs] <- concord[names(dots)[pqs]]
    do.call(f, dots)
  }
}

#' Grouped operator
#'
#' Make a function applicable to grouped data.
#'
#' @param f A function.
#' @param ... Deprecated. Additional arguments to `f` that should *not* be
#' treated as grouped.
#'
#' @returns
#' A function like `f` with a new argument `group`. This accepts a factor to
#' split all other arguments in `f` into groups before applying `f` to each
#' group and combining the results. It is similar to [ave()], but more general.
#'
#' @examples
#' # Redistribute weights.
#'
#' x <- 1:6
#' w <- c(1:5, NA)
#' f <- factor(rep(letters[1:2], each = 3))
#' w1 <- c(2, 4)
#' w2 <- 1:6
#'
#' harmonic_mean(mapply(harmonic_mean, split(x, f), split(w2, f)), w1)
#'
#' wr <- grouped(scale_weights)(w2, group = f) * w1[f]
#' harmonic_mean(x, wr)
#'
#' @family operators
#' @export
grouped <- function(f, ...) {
  f <- match.fun(f)
  ngargs <- list(...)
  if (length(ngargs) > 0L) {
    warning(
      "'...' is depecated and will be removed in a future version; ",
      "use an anonymous function instead"
    )
  }
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
    # Same as unsplit(), but keeps names.
    x <- res[[1L]][rep.int(NA_integer_, length(group))]
    split(x, group) <- res
    if (!is.null(names(x))) {
      split(names(x), group) <- lapply(res, names)
    }
    x
  }
}

#' Balanced operator
#'
#' Makes a function balance the removal of `NA`s across multiple input vectors.
#'
#' @param f A function.
#' @param ... Deprecated. Additional arguments to `f` that should *not* be
#' balanced.
#'
#' @returns
#' A function like `f` with a new argument `na.rm`. If `na.rm = TRUE` then
#' [complete.cases()] is used to remove missing values across all inputs
#' prior to calling `f`.
#'
#' @examples
#' p2 <- price6[[3]]
#' p1 <- price6[[2]]
#' q2 <- quantity6[[3]]
#' q1 <- quantity6[[2]]
#'
#' # Balance missing values for a Fisher index.
#'
#' fisher <- balanced(fisher_index)
#' fisher(p2, p1, q2, replace(q1, 3, NA), na.rm = TRUE)
#' fisher_index(p2[-3], p1[-3], q2[-3], q1[-3])
#'
#' @family operators
#' @export
balanced <- function(f, ...) {
  f <- match.fun(f)
  nbargs <- list(...)
  if (length(nbargs) > 0L) {
    warning(
      "'...' is depecated and will be removed in a future version; ",
      "use an anonymous function instead"
    )
  }

  function(..., na.rm) {
    dots <- list(...)
    if (na.rm) {
      dots <- lapply(dots, `[`, stats::complete.cases(...))
    }
    do.call(f, c(dots, nbargs))
  }
}
