\name{contributions}
\alias{contributions}
\alias{contributions_arithmetic}
\alias{contributions_geometric}
\alias{contributions_harmonic}
\alias{contributions_nested}
\alias{contributions_nested2}

\title{
Additive quote contributions
}

\description{
Calculate additive quote contributions for generalized price indexes, and indexes that nest two levels of generalized means, like the Fisher index, consisting of an outer generalized mean and two inner generalized means.
}

\usage{
contributions(r)

contributions_arithmetic(x, w = rep(1, length(x)))

contributions_geometric(x, w = rep(1, length(x)))

contributions_harmonic(x, w = rep(1, length(x)))

contributions_nested(r, s)

contributions_nested2(r, s)
}

\arguments{
\item{r}{A finite number giving the order of the (outer) generalized mean.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{s}{A pair of finite numbers giving the order of the inner generalized means.}
}

\details{
The function \code{contributions()} is a simple wrapper for \code{weights_transmute(r, 1)()} to calculate (additive) quote contributions for a price index. It returns a function to compute a vector \code{v(x, w)} such that

\code{mean_generalized(r)(x, w) - 1 == sum(v(x, w))}.

That is, \code{v(x, w)} gives the additive contribution for each element of \code{x} in an r-generalized mean. The \code{contributions_arithmetic()}, \code{contributions_geometric()} and \code{contributions_harmonic()} functions cover the most important cases. This generalizes the approach for calculating quote contributions in section 4.2 of Balk (2008).

The \code{contributions_nested()} and \code{contributions_nested2()} functions are the analog of \code{contributions()} for a nested generalized mean with two levels, like a Fisher or Harmonic Laspeyres Paasche index. That is, they return a function that calculates the contribution of each element of \code{x} when a generalized mean of order \code{r} aggregates two generalized means of \code{x} with orders \code{s}, and weights \code{w1} and \code{w2}.

Unlike the case of a generalized index, there are several ways to make contributions for an index based on nested generalized mean. \code{contributions_nested()} uses a generalization of the algorithm in section 6 of Reisdorf et al. (2002), whereas \code{contributions_nested2()} takes a slightly more general approach by taking a weighed average of the contributions for both of the inner means. In most cases the results are broadly similar.
}

\value{
\code{contributions()} returns a function: 

\code{function(x, w = rep(1, length(x))}.

This function returns the additive contribution for each element in \code{x}.

\code{contributions_arithmetic()}, \code{contributions_geometric()}, and \code{contributions_harmonic()} return a numeric vector the same length as \code{x}.

\code{contributions_nested()} and \code{contributions_nested2()} return a function:

\code{function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)))}.

This function takes a numeric vector \code{x} and a pair of numeric weights \code{w1} and \code{w2}, and returns the contribution for each element in \code{x}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive decompositions for Fisher, Tornqvist and geometric mean indexes. \emph{Journal of Economic and Social Measurement}, 28(1-2):51--61.
}

\seealso{
\code{\link{weights_transmute}} for the underlying implementation.
}

\examples{
x <- 2:3
contributions_geometric(x)

# Not the only way to calculate contributions

transmute2 <- function(x) {
  m <- mean_geometric(x)
  (m - 1) / log(m) * log(x) / (x - 1) / length(x)
}

transmute2(x) * (x - 1) # this isn't proportional to the method above
all.equal(sum(transmute2(x) * (x - 1)), mean_geometric(x) - 1)

# But these "transmuted" weights don't recover the geometric mean!
# Not a particularly good way to calculate contributions

isTRUE(all.equal(mean_arithmetic(x, transmute2(x)), mean_geometric(x)))

# There are infinitely many ways to calculate contributions, but the weights
# from weights_transmute(0, 1)() are the *unique* weights that recover the 
# geometric mean

perturb <- function(w, e) {
  w + c(e, -e) / (x - 1)
}

perturb(transmute2(x), 0.1) * (x - 1)
all.equal(sum(perturb(transmute2(x), 0.1) * (x - 1)),
          mean_geometric(x) - 1)
isTRUE(all.equal(mean_arithmetic(x, perturb(transmute2(x), 0.1)),
                 mean_geometric(x)))

#--------------------

p1 <- price6[[2]]
p0 <- price6[[1]]
q1 <- quantity6[[2]]
q0 <- quantity6[[1]]

# Quote contributions for the Fisher index in section 6 of Reinsdorf et al. (2002)

contributions_fisher <- contributions_nested(0, c(1, -1))
(con <- contributions_fisher(p1 / p0, 
                             index_weights("Laspeyres")(p0, q0), 
                             index_weights("Paasche")(p1, q1)))

all.equal(sum(con), index_fisher(p1, p0, q1, q0) - 1)

# Not the only way

contributions_fisher2 <- contributions_nested2(0, c(1, -1))
(con2 <- contributions_fisher2(p1 / p0, 
                               index_weights("Laspeyres")(p0, q0), 
                               index_weights("Paasche")(p1, q1)))

all.equal(sum(con2), index_fisher(p1, p0, q1, q0) - 1)

# The same as the decomposition in section 4.2.2 of Balk (2008)

Qf <- quantity_index(index_fisher)(q1, q0, p1, p0)
Ql <- quantity_index(index_laspeyres)(q1, q0, p0)
wl <- index_weights("Laspeyres")(p0, q0)
wp <- index_weights("HybridPaasche")(p0, q1)

(Qf / (Qf + Ql) * weights_scale(wl) + Ql / (Qf + Ql) * weights_scale(wp)) * (p1 / p0 - 1)

#--------------------

# Works for other types of indexes, too
                                   
contributions_hlp <- contributions_nested(-1, c(1, -1))
contributions_hlp(p1 / p0, 
                  index_weights("Laspeyres")(p0, q0), 
                  index_weights("Paasche")(p1, q1))
}