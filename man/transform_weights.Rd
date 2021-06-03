\name{transform weights}
\alias{transmute_weights}
\alias{factor_weights}
\alias{update_weights}
\alias{scale_weights}

\title{Transform weights}

\description{
Useful transformations for the weights in a generalized mean. 
\itemize{
\item{Transmute weights to turn an r-generalized mean into an s-generalized mean. Useful for calculating additive and multiplicative decompositions for a generalized index.}
\item{Factor weights to turn the generalized mean of a product into the product of generalized means. Useful for price-updating the weights in a generalized index.}
\item{Scale weights so they sum to 1.}
}
}

\usage{
transmute_weights(r, s)

factor_weights(r)

update_weights(x, w = rep(1, length(x)))

scale_weights(x)
}

\arguments{
\item{r, s}{A finite number giving the order of the generalized mean. See details.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}
}

\details{
The function \code{transmute_weights()} returns a function to compute a vector of weights \code{v(x, w)} such that

\preformatted{generalized_mean(r)(x, w) == generalized_mean(s)(x, v(x, w))}

This generalizes the result for turning a geometric mean into an arithmetic mean (and vice versa) in section 4.2 of Balk (2008), although this is usually the most important case.

The function \code{factor_weights()} returns a function to compute weights \code{u(x, w)} such that 

\preformatted{
generalized_mean(r)(x * y, w) == generalized_mean(r)(x, w) * 
                                 generalized_mean(r)(y, u(x, w))
}

This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for chaining the Young index, and gives a way to chain generalized price indexes over time. Factoring weights with \code{r = 1} sometimes gets called price-updating weights; \code{update_weights()} simply calls \code{factor_weights(1)()}.

The function \code{scale_weights()} scales a vector of weights so they sum to 1 by calling \code{x / sum(x, na.rm = TRUE)}.

Both \code{x} and \code{w} should be strictly positive. This is not enforced, but the results may not make sense in cases where the generalized mean and generalized logarithmic mean are not defined.
}

\value{
\code{transmute_weights()} and \code{factor_weights()} return a function: 

\code{function(x, w = rep(1, length(x))}.

In each case this function augments the weights \code{w}.

\code{update_weights()} and \code{scale_weights()} return a numeric vector the same length as \code{x}.
}

\note{
Transmuting, factoring, and scaling weights will return a value that is the same length as \code{x}, so any NAs in \code{x} or \code{w} will return NA. Unless all values are NA, however, the result for transmuting or factoring will still satisfy the above identities when \code{na.rm = TRUE} in \code{\link[=generalized_mean]{generalized_mean()}}. Similarly, the result of scaling will sum to 1 when NAs are removed.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.

Sydsaeter, K., Strom, A., and Berck, P. (2005). \emph{Economists' Mathematical Manual} (4th edition). Springer.
}

\seealso{
\code{\link{generalized_mean}} for the generalized mean. 

\code{\link{extended_mean}} for the extended mean that underlies \code{transmute_weights()}.

\code{\link{contributions}} for calculating additive quote contributions.
}

\examples{
x <- 2:3
y <- 4:5
w <- runif(2)

#---- Transforming generalized means ----

# Calculate the geometric mean as an arithmetic mean and harmonic mean by 
# transmuting the weights

geometric_mean(x)
arithmetic_mean(x, transmute_weights(0, 1)(x))
harmonic_mean(x, transmute_weights(0, -1)(x))

# Works for nested means, too

w1 <- runif(2)
w2 <- runif(2)

geometric_mean(c(arithmetic_mean(x, w1), harmonic_mean(x, w2)))

v1 <- scale_weights(transmute_weights(1, 0)(x, w1))
v2 <- scale_weights(transmute_weights(-1, 0)(x, w2))
arithmetic_mean(x, transmute_weights(0, 1)(x, 0.5 * v1 + 0.5 * v2))

#---- Quote contributions ----

# Transmuted weights can be used to calculate quote contributions for, 
# e.g., a geometric price index

scale_weights(transmute_weights(0, 1)(x)) * (x - 1)
geometric_contributions(x) # the more convenient way

#---- Basket representation of a price index ----

# Any generalized index can be represented as a basket-style index
# by transmuting the weights, which is how some authors define a
# price index (e.g., Sydsaeter et al., 2005, p. 174)

p1 <- 2:6
p0 <- 1:5

qs <- transmute_weights(-1, 1)(p1 / p0) / p0
all.equal(harmonic_mean(p1 / p0), sum(p1 * qs) / sum(p0 * qs))

#---- Factoring the product of generalized means ----

# Factor the harmonic mean by chaining the calculation

harmonic_mean(x * y, w)
harmonic_mean(x, w) * harmonic_mean(y, factor_weights(-1)(x, w))

# The common case of an arithmetic mean

arithmetic_mean(x * y, w)
arithmetic_mean(x, w) * arithmetic_mean(y, update_weights(x, w))

# In cases where x and y have the same order, Chebyshev's inequality implies
# that the chained calculation is too small

arithmetic_mean(x * y, w) > arithmetic_mean(x, w) * arithmetic_mean(y, w)
}