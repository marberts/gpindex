\name{lehmer mean}
\alias{mean_lehmer}
\alias{mean_contraharmonic}

\title{Lehmer mean}

\description{
Calculate a Lehmer mean.
}

\usage{
mean_lehmer(r)

mean_contraharmonic(x, w = rep(1, length(x)), na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the Lehmer mean.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed?}
}

\value{
\code{mean_lehmer()} returns a function:

\code{function(x, w = rep(1, length(x)), na.rm = FALSE)}. 

\code{mean_contraharmonic()} returns a numeric value.
}

\details{
The function \code{mean_lehmer()} returns a function to compute the Lehmer mean of order \code{r} of \code{x} with weights \code{w}, which is calculated as the arithmetic mean of \code{x} with weights \code{w * x^(r-1)}. This is also called the counter-harmonic mean. See Bullen (2003, p. 245) for a definition, or \url{https://en.wikipedia.org/wiki/Lehmer_mean}.

The Lehmer mean of order 2 is sometimes called the contraharmonic mean. The function \code{mean_contraharmonic()} simply calls \code{mean_lehmer(2)()}. Like the generalized mean, the contraharmonic mean is the solution to an optimal prediction problem: choose \eqn{m}{m} to minimize \eqn{\sum_{i = 1}^{n} w_{i} \left(\frac{x_{i}}{m} - 1 \right)^2}{\sum w (x / m - 1)^2}. The Lehmer mean of order -1 has a similar interpretation, replacing \eqn{\frac{x_{i}}{m}}{x / m} with \eqn{\frac{m}{x_{i}}}{m / x}, and together these bound the harmonic and arithmetic means.

Both \code{x} and \code{w} should be strictly positive. This is not enforced, but the results may not make sense if the Lehmer mean in not defined. 

The Lehmer mean is an alternative to the generalized mean that generalizes the Pythagorean means. The function \code{mean_lehmer(1)()} is identical to \code{mean_artithmetic()}, \code{mean_lehmer(0)()} is identical to \code{mean_harmonic()}, and \code{mean_lehmer(0.5)()} is identical to \code{mean_geometric()} with two values and no weights.
}

\note{
\code{mean_lehmer()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[=min]{min()}}/\code{\link[=max]{max()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite.
}

\references{
Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer Science+Business Media.

Lehmer, D. H. (1971). On the Compounding of Certain Means. \emph{Journal of Mathematical Analysis and Applications}, 36(1): 183-200.
}

\seealso{
\code{\link{mean_generalized}} for the generalized mean, an alternative to the Lehmer mean.

\code{\link{logmean_generalized}} for the generalized logarithmic mean. 
}

\examples{
# Make some data

x <- 2:3
w <- c(0.25, 0.75)

# The Pythagorean means are special cases of the Lehmer mean

all.equal(mean_lehmer(1)(x, w), mean_arithmetic(x, w))
all.equal(mean_lehmer(0)(x, w), mean_harmonic(x, w))
all.equal(mean_lehmer(0.5)(x), mean_geometric(x))

#--------------------

# When r < 1, the generalized mean is larger than the corresponding
# Lehmer mean

mean_lehmer(-1)(x, w) < mean_generalized(-1)(x, w)

# The reverse is true when r > 1

mean_lehmer(3)(x, w) > mean_generalized(3)(x, w)

# This implies the contraharmonic mean is larger than the quadratic
# mean, and therefore the Pythagorean means

mean_contraharmonic(x, w) > mean_arithmetic(x, w)
mean_contraharmonic(x, w) > mean_geometric(x, w)
mean_contraharmonic(x, w) > mean_harmonic(x, w)

# and the logarithmic mean

mean_contraharmonic(2:3) > logmean(2, 3)

# The difference between the arithmetic mean and contraharmonic mean
# is proportional to the variance of x

weighted_var <- function(x, w) mean_arithmetic(x^2, w) - mean_arithmetic(x, w)^2

mean_arithmetic(x, w) + weighted_var(x, w) / mean_arithmetic(x, w)
mean_contraharmonic(x, w)

#--------------------

# It is easy to modify the weights to turn a Lehmer mean of order r
# into a Lehmer mean of order s because the Lehmer mean can be expressed
# as an arithmetic mean

r <- 2
s <- -3
mean_lehmer(r)(x, w)
mean_lehmer(s)(x, w * x^(r - 1) / x^(s - 1))

# The weights can also be modified to turn a Lehmer mean of order r
# into a generalized mean of order s

mean_lehmer(r)(x, w)
mean_generalized(s)(x, weights_transmute(1, s)(x, w * x^(r - 1)))

# and vice versa

mean_lehmer(r)(x, weights_transmute(s, 1)(x, w) / x^(r - 1))
mean_generalized(s)(x, w)

#--------------------

# Quote contributions for a price index based on the Lehmer mean
# are easy to calculate

weights_scale(w * x^(r - 1)) * (x - 1)
}