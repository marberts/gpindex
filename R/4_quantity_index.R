quantity_index <- function(price_index) {
  stopifnot("'price_index' must be a function" = is.function(price_index))
  args <- formals(price_index)
  concord <- c(p1 = "q1", p0 = "q0", q1 = "p1", q0 = "p0", pb = "qb", qb = "pb")
  pqs <- intersect(names(args), concord)
  if (!length(pqs)) stop("No price/quantity arguments")
  qps <- concord[pqs]
  not_pqs <- setdiff(names(args), concord)
  #names(not_pqs) <- not_pqs
  res <- function() {}
  names(args)[names(args) %in% pqs] <- qps
  formals(res) <- args
  body(res)[[2]] <- as.call(c(price_index, lapply(c(qps, not_pqs), as.name)))
  # clean up enclosing environment
  environment(res) <- getNamespace("gpindex")
  res
}
