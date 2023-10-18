#' Quantity index
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
#' # Remap argument names to be quantities rather than prices
#' 
#' quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)
#' 
#' laspeyres_index(p1 = q1, p0 = q0, q0 = p0)
#' 
#' # Works with the index_weights() functions, too
#' 
#' quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
#' 
#' @family operators
#' @export
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

#' Grouped
#' 
#' Make a function applicable to grouped data.
#' 
#' @param f A function.
#' @param ... Additional arguments to `f` that should *not* be treated as
#' grouped.
#' 
#' @returns
#' A function like `f` with a new argument
#' `group`. This accepts a factor to split all other arguments in `f`
#' (except those specified in `...`) into groups before applying `f`
#' to each group and combining the results. It is similar to
#' [ave()], but more general.
#' 
#' @note
#' Additional arguments passed to `...` are evaluated, so the return
#' function doesn't evaluate these arguments lazily. In most cases these
#' additional arguments are parameters like `elasticity` or switches like
#' `na.rm`, and eager evaluation prevents these from being changed in,
#' e.g., a loop. Lazy evaluation can be had by passing an anonymous function
#' that partials out these arguments.
#' 
#' @examples
#' p1 <- price6[[3]]
#' p0 <- price6[[2]]
#' q1 <- quantity6[[3]]
#' q0 <- quantity6[[2]]
#' 
#' # Calculate Tornqvist weights for two groups
#' 
#' f <- factor(rep(letters[1:2], each = 3))
#' tornqvist_weights <- grouped(index_weights("Tornqvist"))
#' tornqvist_weights(p1, p0, q1, q0, group = f)
#' 
#' # Calculate a mean like ave(), but with weights
#' 
#' x <- 1:6
#' w <- c(1:5, NA)
#' grouped_mean <- grouped(geometric_mean, na.rm = TRUE)
#' grouped_mean(x, w, group = f)
#' 
#' # Redistribute weights
#' 
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

#' Balanced
#'
#' Makes a function balance the removal of `NA`s across multiple input vectors.
#'
#' @param f A function.
#' @param ... Additional arguments to `f` that should *not* be balanced.
#'
#' @returns
#' A function like `f`, except that missing values are balanced across
#' *all* inputs when `na.rm = TRUE` (except those specified in `...`). This
#' is like using [complete.cases()] on the inputs of `f`.
#'
#' @inherit grouped note
#' 
#' @examples
#' p1 <- price6[[3]]
#' p0 <- price6[[2]]
#' q1 <- quantity6[[3]]
#' q0 <- quantity6[[2]]
#' 
#' # Balance missing values for a Fisher index
#' 
#' fisher <- balanced(fisher_index)
#' fisher(p1, p0, q1, replace(q0, 3, NA), na.rm = TRUE)
#' fisher_index(p1[-3], p0[-3], q1[-3], q0[-3])
#' 
#' # Operators can be combined, but some care may be needed
#'
#' x <- 1:6
#' w <- c(1:5, NA)
#' 
#' f <- factor(rep(letters[1:2], each = 3))
#'
#' grouped(balanced(fisher_mean), na.rm = TRUE)(x, w, group = f)
#' balanced(grouped(fisher_mean))(x, w, group = f, na.rm = TRUE)
#'
#' @family operators
#' @export
balanced <- function(f, ...) {
  f <- match.fun(f)
  nbargs <- list(...)

  function(..., na.rm = FALSE) {
    dots <- list(...)
    if (na.rm) {
      dots <- lapply(dots, `[`, stats::complete.cases(...))
    }
    do.call(f, c(dots, nbargs))
  }
}
