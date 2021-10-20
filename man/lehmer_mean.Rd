\name{lehmer_mean}
\alias{lehmer_mean}
\alias{contraharmonic_mean}

\title{Lehmer mean}

\description{
Calculate a Lehmer mean.
}

\usage{
## Function factory
lehmer_mean(r)

## Special case
contraharmonic_mean(x, w, na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the Lehmer mean.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed? By default missing values in \code{x} or \code{w} return a missing value.}
}

\details{
The function \code{lehmer_mean()} returns a function to compute the Lehmer mean of order \code{r} of \code{x} with weights \code{w}, which is calculated as the arithmetic mean of \code{x} with weights \eqn{wx^{r-1}}{wx^(r-1)}. This is also called the counter-harmonic mean. See Bullen (2003, p. 245) for a definition, or \url{https://en.wikipedia.org/wiki/Lehmer_mean}.

The Lehmer mean of order 2 is sometimes called the contraharmonic mean. The function \code{contraharmonic_mean()} simply calls \code{lehmer_mean(2)()}. Like the generalized mean, the contraharmonic mean is the solution to an optimal prediction problem: choose \eqn{m}{m} to minimize \eqn{\sum_{i = 1}^{n} w_{i} \left(\frac{x_{i}}{m} - 1 \right)^2}{\sum w (x / m - 1)^2}. The Lehmer mean of order -1 has a similar interpretation, replacing \eqn{\frac{x_{i}}{m}}{x / m} with \eqn{\frac{m}{x_{i}}}{m / x}, and together these bound the harmonic and arithmetic means.

Both \code{x} and \code{w} should be strictly positive. This is not enforced, but the results may not make sense if the generalized mean in not defined. 

The Lehmer mean is an alternative to the generalized mean that generalizes the Pythagorean means. The function \code{lehmer_mean(1)()} is identical to \code{arithmetic_mean()}, \code{lehmer_mean(0)()} is identical to \code{harmonic_mean()}, and \code{lehmer_mean(0.5)()} is identical to \code{geometric_mean()} with two values and no weights.
}

\value{
\code{lehmer_mean()} returns a function:

\preformatted{function(x, w, na.rm = FALSE){...}}

This computes the Lehmer mean of order \code{r} of \code{x} with weights \code{w}.

\code{contraharmonic_mean()} returns a numeric value for the Lehmer mean of order 2.
}

\note{
\code{lehmer_mean()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[=min]{min()}}/\code{\link[=max]{max()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite.
}

\references{
Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer Science+Business Media.

Lehmer, D. H. (1971). On the Compounding of Certain Means. \emph{Journal of Mathematical Analysis and Applications}, 36(1): 183-200.
}

\seealso{
\code{\link{generalized_mean}} for the generalized mean, an alternative to the Lehmer mean.

\code{\link{generalized_logmean}} for the generalized logarithmic mean. 
}

\examples{
x <- 2:3
w <- c(0.25, 0.75)

#---- The Pythagorean means are special cases of the Lehmer mean ----

all.equal(lehmer_mean(1)(x, w), arithmetic_mean(x, w))
all.equal(lehmer_mean(0)(x, w), harmonic_mean(x, w))
all.equal(lehmer_mean(0.5)(x), geometric_mean(x))

#---- Comparing Lehmer means and generalized means ----

# When r < 1, the generalized mean is larger than the 
# corresponding Lehmer mean

lehmer_mean(-1)(x, w) < generalized_mean(-1)(x, w)

# The reverse is true when r > 1

lehmer_mean(3)(x, w) > generalized_mean(3)(x, w)

# This implies the contraharmonic mean is larger than the 
# quadratic mean, and therefore the Pythagorean means

contraharmonic_mean(x, w) > arithmetic_mean(x, w)
contraharmonic_mean(x, w) > geometric_mean(x, w)
contraharmonic_mean(x, w) > harmonic_mean(x, w)

# and the logarithmic mean

contraharmonic_mean(2:3) > logmean(2, 3)

# The difference between the arithmetic mean and 
# contraharmonic mean is proportional to the variance of x

weighted_var <- function(x, w) {
  arithmetic_mean(x^2, w) - arithmetic_mean(x, w)^2
} 

arithmetic_mean(x, w) + weighted_var(x, w) / arithmetic_mean(x, w)
contraharmonic_mean(x, w)

#---- Changing the order of the mean ----

# It is easy to modify the weights to turn a Lehmer mean of order r
# into a Lehmer mean of order s because the Lehmer mean can be 
# expressed as an arithmetic mean

r <- 2
s <- -3
lehmer_mean(r)(x, w)
lehmer_mean(s)(x, w * x^(r - 1) / x^(s - 1))

# The weights can also be modified to turn a Lehmer mean of order r
# into a generalized mean of order s

lehmer_mean(r)(x, w)
generalized_mean(s)(x, transmute_weights(1, s)(x, w * x^(r - 1)))

# and vice versa

lehmer_mean(r)(x, transmute_weights(s, 1)(x, w) / x^(r - 1))
generalized_mean(s)(x, w)

#---- Quote contributions ----

# Quote contributions for a price index based on the Lehmer mean
# are easy to calculate

scale_weights(w * x^(r - 1)) * (x - 1)
}