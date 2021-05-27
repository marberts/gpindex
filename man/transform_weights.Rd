\name{transform weights}
\alias{weights_transmute}
\alias{weights_factor}
\alias{weights_update}
\alias{weights_scale}

\title{Transform weights}

\description{
Useful transformations for the weights in a generalized mean. 
\itemize{
\item{Transmute weights to turn an r-generalized mean into an s-generalized mean. Useful for calculating the additive contribution of each quote in a generalized price index.}
\item{Factor weights to turn the generalized mean of a product into the product of generalized means. Useful for price-updating the weights in a generalized price index.}
\item{Scale weights so they sum to 1.}
}
}

\usage{
weights_transmute(r, s)

weights_factor(r)

weights_update(x, w = rep(1, length(x)))

weights_scale(x)
}

\arguments{
\item{r, s}{A number giving the order of the generalized mean. See details.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}
}

\value{
\code{weights_transmute()} and \code{weights_factor()} return a function: 

\code{function(x, w = rep(1, length(x))}.

\code{weights_update()} and \code{weights_scale()} return a numeric vector the same length as \code{x}.
}

\details{
Both \code{x} and \code{w} should be strictly positive. This is not enforced, but the results may not make sense in cases where the generalized mean and generalized logarithmic mean are not defined.

\bold{Transmute weights}
The function \code{weights_transmute()} returns a function to compute a vector of weights \code{v(x, w)} such that

\code{mean_generalized(r)(x, w) == mean_generalized(s)(x, v(x, w))}. 

These weights are calculated as 

\code{v(x, w) = w * mean_extended(r, s)(x, mean_generalized(r)(x, w))^(r - s)}.

This generalizes the result for turning a geometric mean into an arithmetic mean (and vice versa) in section 4.2 of Balk (2008), although this is usually the most important case.

\bold{Factor weights}
The function \code{weights_factor()} returns a function to compute weights \code{u(x, w)} such that 

\code{mean_generalized(r)(x * y, w) == mean_generalized(r)(x, w) * mean_generalized(r)(y, u(x, w))}. 

These weights are calculated as \code{u(x, w) = w * x^r}.

This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for chaining the Young index, and gives a way to chain generalized price indexes over time. Factoring weights with \code{r = 1} sometimes gets called price-updating weights; \code{weights_update()} simply calls \code{weights_factor(1)()}.

\bold{Scale weights}
The function \code{weights_scale()} scales a vector of weights so they sum to 1 by calling \code{x / sum(x, na.rm = TRUE)}.
}

\note{
Transmuting, factoring, and scaling weights will return a value that is the same length as \code{x}, so any NAs in \code{x} or \code{w} will return NA. Unless all values are NA, however, the result for transmuting or factoring will still satisfy the above identities when \code{na.rm = TRUE} in \code{\link[=mean_generalized]{mean_generalized()}}. Similarly, the result of scaling will sum to 1 when NAs are removed.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.

Sydsaeter, K., Strom, A., and Berck, P. (2005). \emph{Economists' Mathematical Manual} (4th edition). Springer.
}

\seealso{
\code{\link{mean_generalized}} for the generalized mean. 

\code{\link{mean_extended}} for the extended mean that underlies \code{weights_transmute()}.

\code{\link{contributions}} for calculating additive quote contributions.
}

\examples{
# Make some data

x <- 2:3
y <- 4:5
w <- runif(2)

# Calculate the geometric mean as an arithmetic mean and harmonic mean by 
# transmuting the weights

mean_geometric(x)
mean_arithmetic(x, weights_transmute(0, 1)(x))
mean_harmonic(x, weights_transmute(0, -1)(x))

# Works for nested means, too

w1 <- runif(2)
w2 <- runif(2)

mean_geometric(c(mean_arithmetic(x, w1), mean_harmonic(x, w2)))

v1 <- weights_scale(weights_transmute(1, 0)(x, w1))
v2 <- weights_scale(weights_transmute(-1, 0)(x, w2))
mean_arithmetic(x, weights_transmute(0, 1)(x, 0.5 * v1 + 0.5 * v2))

#--------------------

# Transmuted weights can be used to calculate quote contributions for, 
# e.g., a geometric price index

weights_scale(weights_transmute(0, 1)(x)) * (x - 1)
contributions_geometric(x) # the more convenient way

#--------------------

# Any generalized index can be represented as a basket-style index
# by transmuting the weights, which is how some authors define a
# price index (e.g., Sydsaeter et al., 2005, p. 174)

p1 <- 2:6
p0 <- 1:5

qs <- weights_transmute(-1, 1)(p1 / p0) / p0
all.equal(mean_harmonic(p1 / p0), sum(p1 * qs) / sum(p0 * qs))

#--------------------

# Factor the harmonic mean by chaining the calculation

mean_harmonic(x * y, w)
mean_harmonic(x, w) * mean_harmonic(y, weights_factor(-1)(x, w))

# The common case of an arithmetic mean

mean_arithmetic(x * y, w)
mean_arithmetic(x, w) * mean_arithmetic(y, weights_update(x, w))

# In cases where x and y have the same order, Chebyshev's inequality implies
# that the chained calculation is too small

mean_arithmetic(x * y, w) > mean_arithmetic(x, w) * mean_arithmetic(y, w)
}