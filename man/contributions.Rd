\name{contributions}
\alias{contributions}
\alias{arithmetic_contributions}
\alias{geometric_contributions}
\alias{harmonic_contributions}
\alias{nested_contributions}
\alias{nested_contributions2}

\title{
Additive quote contributions
}

\description{
Calculate additive quote contributions for generalized price indexes, and indexes that nest two levels of generalized means (e.g., the Fisher index) consisting of an outer generalized mean and two inner generalized means.
}

\usage{
contributions(r)

arithmetic_contributions(x, w = rep(1, length(x)))

geometric_contributions(x, w = rep(1, length(x)))

harmonic_contributions(x, w = rep(1, length(x)))

nested_contributions(r, s, t = c(1, 1))

nested_contributions2(r, s, t = c(1, 1))
}

\arguments{
\item{r}{A finite number giving the order of the generalized mean.}

\item{x}{A strictly positive numeric vector of price relatives.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{s}{A pair of finite numbers giving the order of the inner generalized means.}

\item{t}{A pair of strictly positive weights for the inner generalized means. The default is equal weights.}
}

\details{
The function \code{contributions()} is a simple wrapper for \code{\link[=transmute_weights]{transmute_weights(r, 1)()}} to calculate (additive) quote contributions for a price index based on a generalized mean of order \code{r}. It returns a function to compute a vector \code{v(x, w)} such that \code{generalized_mean(r)(x, w) - 1 == sum(v(x, w))}. This generalize the approach for calculating quote contributions in section 4.2 of Balk (2008). The \code{arithmetic_contributions()}, \code{geometric_contributions()} and \code{harmonic_contributions()} functions cover the most important cases. 

The \code{nested_contributions()} and \code{nested_contributions2()} functions are the analog of \code{contributions()} for an index based on a nested generalized mean with two levels, like a Fisher or Harmonic Laspeyres Paasche index. They return a function that calculates the contribution of each element of \code{x} when a generalized mean of order \code{r} aggregates two generalized indexes of \code{x} with orders \code{s}, and weights \code{w1} and \code{w2}.

Unlike the case of a generalized index, there are several ways to make contributions for an index based on nested generalized means. \code{nested_contributions()} uses a generalization of the algorithm in section 6 of Reinsdorf et al. (2002), whereas \code{nested_contributions2()} takes a slightly more general approach by constructing a weighed average of the contributions for both of the inner means. In most cases the results are broadly similar.
}

\value{
\code{contributions()} returns a function: 

\code{function(x, w = rep(1, length(x))}.

This computes the additive contribution for each element in \code{x} in an index based on the generalized mean of order \code{r}.

\code{arithmetic_contributions()}, \code{geometric_contributions()}, and \code{harmonic_contributions()} each return a numeric vector, the same length as \code{x}, giving the contribution of each element of \code{x} in an arithmetic, geometric, or harmonic index.

\code{nested_contributions()} and \code{nested_contributions2()} return a function:

\code{function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)))}.

This function takes a numeric vector \code{x} and a pair of numeric weights \code{w1} and \code{w2}, and returns the contribution for each element of \code{x}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive decompositions for Fisher, Tornqvist and geometric mean indexes. \emph{Journal of Economic and Social Measurement}, 28(1-2):51--61.
}

\seealso{
\code{\link{transmute_weights}} for the underlying implementation.
}

\examples{
x <- 2:3

#---- Contributions for a geometric index ----

geometric_contributions(x)

# Not the only way to calculate contributions

transmute2 <- function(x) {
  m <- geometric_mean(x)
  (m - 1) / log(m) * log(x) / (x - 1) / length(x)
}

transmute2(x) * (x - 1) # this isn't proportional to the method above
all.equal(sum(transmute2(x) * (x - 1)), geometric_mean(x) - 1)

# But these "transmuted" weights don't recover the geometric mean!
# Not a particularly good way to calculate contributions

isTRUE(all.equal(arithmetic_mean(x, transmute2(x)), geometric_mean(x)))

# There are infinitely many ways to calculate contributions, but the weights
# from transmute_weights(0, 1)() are the *unique* weights that recover the 
# geometric mean

perturb <- function(w, e) {
  w + c(e, -e) / (x - 1)
}

perturb(transmute2(x), 0.1) * (x - 1)
all.equal(sum(perturb(transmute2(x), 0.1) * (x - 1)),
          geometric_mean(x) - 1)
isTRUE(all.equal(arithmetic_mean(x, perturb(transmute2(x), 0.1)),
                 geometric_mean(x)))

#---- Contributions for a Fisher index ----

p1 <- price6[[2]]
p0 <- price6[[1]]
q1 <- quantity6[[2]]
q0 <- quantity6[[1]]

# Quote contributions for the Fisher index in section 6 of Reinsdorf et al. (2002)

contributions_fisher <- nested_contributions(0, c(1, -1))
(con <- contributions_fisher(p1 / p0, 
                             index_weights("Laspeyres")(p0, q0), 
                             index_weights("Paasche")(p1, q1)))

all.equal(sum(con), index_fisher(p1, p0, q1, q0) - 1)

# Not the only way

contributions_fisher2 <- nested_contributions2(0, c(1, -1))
(con2 <- contributions_fisher2(p1 / p0, 
                               index_weights("Laspeyres")(p0, q0), 
                               index_weights("Paasche")(p1, q1)))

all.equal(sum(con2), index_fisher(p1, p0, q1, q0) - 1)

# The same as the van IJzeren decomposition in section 4.2.2 of Balk (2008)

Qf <- quantity_index(index_fisher)(q1, q0, p1, p0)
Ql <- quantity_index(index_laspeyres)(q1, q0, p0)
wl <- index_weights("Laspeyres")(p0, q0)
wp <- index_weights("HybridPaasche")(p0, q1)

(Qf / (Qf + Ql) * scale_weights(wl) + Ql / (Qf + Ql) * scale_weights(wp)) * (p1 / p0 - 1)

#---- Contributions for other types of indexes ----

# A function to get contributions for any superlative quadratic mean of order 'r' index

contributions_superlative <- function(r) {
  nested_contributions(0, c(r / 2, - r / 2))
}

# Works for other types of indexes, like the harmonic Laspeyres Paasche index
                                   
contributions_hlp <- nested_contributions(-1, c(1, -1))
contributions_hlp(p1 / p0, 
                  index_weights("Laspeyres")(p0, q0), 
                  index_weights("Paasche")(p1, q1))
                  
# Or the AG mean index

contributions_agm <- nested_contributions(1, c(0, 1), c(0.25, 0.75))
contributions_agm(p1 / p0, 
                  index_weights("Laspeyres")(p0, q0), 
                  index_weights("Laspeyres")(p0, q0))
                  
}