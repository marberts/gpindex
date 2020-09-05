\name{Transform weights}
\alias{weights_transmute}
\alias{weights_g2a}
\alias{weights_h2a}
\alias{weights_factor}
\alias{weights_update}
\alias{weights_scale}

\title{Transformations for weights in a generalized mean}

\description{
Useful transformations for the weights in a generalized mean. 
\itemize{
\item{Transmute weights to turn an r-generalized mean into an s-generalized mean.}
\item{Factor weights to turn the generalized mean of a product into the product of generalized means.}
\item{Scale weights so they sum to 1.}
}
}

\usage{
weights_transmute(r, s)

weights_g2a(x, w = rep(1, length(x)))

weights_h2a(x, w = rep(1, length(x)))

weights_factor(r)

weights_update(x, w = rep(1, length(x)))

weights_scale(x)
}

\arguments{
\item{r, s}{A number giving the exponent of the generalized mean. See details.}

\item{x}{A numeric vector.}

\item{w}{A numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}
}

\value{
\code{weights_transmute()} and \code{weights_factor()} return a function: \code{function(x, w)}; otherwise, a numeric vector the same length as \code{x}.
}

\details{
As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense in cases where the generalized mean and generalized logarithmic mean are not defined.

\bold{Transmute weights}
The function \code{weights_transmute()} returns a function to compute a vector of weights \code{v(x, w)} such that

\code{mean_generalized(r)(x, w) == mean_generalized(s)(x, v(x, w))}. 

These weights are calculated as 

\code{v(x, w) = w * mean_extended(r, s)(x, mean_generalized(r)(x, w))^(r - s)}.

This generalizes the result in section 4.2 of Balk (2008) when \code{r} and \code{s} are 0 or 1, although these are usually the most important cases. The functions \code{weights_g2a()} and \code{weights_h2a()} calculate the weights to turn a geometric and harmonic mean into an arithmetic mean (i.e., setting \code{r = 0} and \code{r = -1} in \code{weights_transmute(r, 1)}).

\bold{Factor weights}
The function \code{weights_factor()} returns a function to compute weights \code{u(x, w)} such that 

\code{mean_generalized(r)(x * y, w) == mean_generalized(r)(x, w) * mean_generalized(r)(y, u(x, w))}. 

These weights are calculated as \code{u(x, w) = w * x^r}.

This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for the Young index. Factoring weights with \code{r = 1} sometimes gets called price-updating weights; \code{weights_update()} simply calls \code{weights_factor(1)}.

\bold{Scale weights}
The function \code{weights_scale()} scales a vector of weights so they sum to 1 by calling \code{x / sum(x, na.rm = TRUE)}.
}

\note{
Transmuting, factoring, and scaling weights will return a value that is the same length as \code{x}, so any NAs in \code{x} or \code{w} will return NA. Unless all values are NA, however, the result for transmuting or factoring will still satisfy the above identities when \code{na.rm = TRUE} in \code{\link[gpindex:mean_generalized]{mean_generalized()}}. Similarly, the result of scaling will sum to 1 when NAs are removed.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{mean_extended}} for the extended mean. 
}

\examples{
# Make some data

x <- 1:10
y <- 11:20
w <- runif(10)

# Calculate the geometric mean as an arithmetic mean and harmonic mean 
# by transmuting the weights

mean_geometric(x)
mean_arithmetic(x, weights_g2a(x))
mean_harmonic(x, weights_transmute(0, -1)(x))

# Works for nested means, too

w1 <- runif(10)
w2 <- runif(10)

mean_geometric(c(mean_arithmetic(x, w1), mean_harmonic(x, w2)))

v0 <- weights_g2a(c(mean_arithmetic(x, w1), mean_harmonic(x, w2)))
v0 <- weights_scale(v0)
v1 <- weights_scale(w1)
v2 <- weights_scale(weights_h2a(x, w2))
mean_arithmetic(x, v0[1] * v1 + v0[2] * v2)

# Factor the harmonic mean

mean_harmonic(x * y, w)
mean_harmonic(x, w) * mean_harmonic(y, weights_factor(-1)(x, w))

# The common case of an arithmetic mean

mean_arithmetic(x * y, w)
mean_arithmetic(x, w) * mean_arithmetic(y, weights_update(x, w))

# In cases where x and y have the same order, Chebyshev's inequality
# implies that the chained calculation is too small

mean_arithmetic(x * y, w) > mean_arithmetic(x, w) * mean_arithmetic(y, w)
}