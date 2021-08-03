quantity_index <- function(f) {
  f <- match.fun(f)
  args <- formals(f)
  concord <- c(p1 = "q1", p0 = "q0", q1 = "p1", q0 = "p0", pb = "qb", qb = "pb")
  pqs <- intersect(names(args), concord)
  if (!length(pqs)) {
    stop(gettext("no price/quantity arguments"))
  }
  qps <- concord[pqs]
  not_pqs <- setdiff(names(args), concord)
  res <- function() {}
  names(args)[names(args) %in% pqs] <- qps
  formals(res) <- args
  body(res)[[2]] <- as.call(c(f, lapply(c(qps, not_pqs), as.name)))
  # clean up enclosing environment
  environment(res) <- getNamespace("gpindex")
  res
}

grouped <- function(f, ...) {
  f <- match.fun(f)
  ngargs <- list(...)
  if ("group" %in% names(formals(f))) {
    stop("'f' already has an argument called 'group'")
  }
  function(..., group) {
    group <- as.factor(group)
    args <- lapply(list(...), split, group)
    res <- .mapply(f, args, ngargs)
    res <- unsplit(res, group)
    attributes(res) <- NULL # unsplit mangles attributes
    res
  }
}
