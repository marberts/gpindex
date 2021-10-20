\name{nested_mean}
\alias{nested_mean}
\alias{fisher_mean}

\title{Nested generalized mean}

\description{
Calculate the (outer) generalized mean of two (inner) generalized means.
}

\usage{
## Function factory
nested_mean(r, s, t = c(1, 1))

## Special case
fisher_mean(x, w1, w2, na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the outer generalized mean.}

\item{s}{A pair of finite numbers giving the order of the inner generalized means.}

\item{t}{A pair of strictly positive weights for the inner generalized means. The default is equal weights.}

\item{x}{A strictly positive numeric vector.}

\item{w1, w2}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x}, \code{w1}, and \code{w2} be removed? By default missing values in \code{x}, \code{w1}, or \code{w2} return a missing value.}
}

\value{
\code{nested_mean()} returns a function:

\preformatted{function(x, w1, w2, na.rm = FALSE){...}}

This computes the generalized mean of order \code{r} of the generalized mean of order \code{s[1]} of \code{x} with weights \code{w1} and the generalized mean of order \code{s[2]} of \code{x} with weights \code{w2}.

\code{fisher_mean()} returns a numeric value for the geometric mean of the arithmetic and harmonic means (i.e., \code{r = 0} and \code{s = c(1, -1)}).
}

\note{
There is some ambiguity about how to remove missing values in \code{w1} or \code{w2} when \code{na.rm = TRUE}. The approach here is to remove missing values when calculating each of the inner means individually, rather than removing all missing values prior to any calculations. This means that a different number of data points could be used to calculate the inner means. Use the \code{\link[=balanced]{balanced()}} operator to balanced missing values across \code{w1} and {w2} prior to any calculations.
}

\references{
Diewert, W. E. (1976). Exact and superlative index numbers. \emph{Journal of Econometrics}, 4(2): 114--145.

Lent, J. and Dorfman, A, H. (2009). Using a weighted average of base period price indexes to approximate a superlative index. \emph{Journal of Official Statistics}, 25(1):139--149.
}

\seealso{
\code{\link{generalized_mean}} for the generalized mean.

\code{\link{nested_contributions}} for quote contributions for indexes based on nested generalized means, like the Fisher index.
}

\examples{
x <- 1:3
w1 <- 4:6
w2 <- 7:9

#---- Making superlative indexes ----

# A function to make the superlative quadratic mean price index 
# by Diewert (1976) as a product of generalized means

quadratic_mean_index <- function(x, w0, w1, r) {
  x <- sqrt(x) 
  generalized_mean(r)(x, w0) * generalized_mean(-r)(x, w1)
}

quadratic_mean_index(x, w1, w2, 2)

# Same as the nested generalized mean (with the order halved)

quadratic_mean_index2 <- function(r) nested_mean(0, c(r / 2, -r / 2))

quadratic_mean_index2(2)(x, w1, w2)

# The arithmetic AG mean index by Lent and Dorfman (2009)

agmean_index <- function(tau) nested_mean(1, c(0, 1), c(tau, 1 - tau))

agmean_index(0.25)(x, w1, w1)

#---- Missing values ----

x[1] <- NA
w1[2] <- NA

fisher_mean(x, w1, w2, na.rm = TRUE)

# Same as using obs 2 and 3 in an arithmetic mean, 
# and obs 3 in a harmonic mean

geometric_mean(c(arithmetic_mean(x, w1, na.rm = TRUE), 
                 harmonic_mean(x, w2, na.rm = TRUE)))

# Use balanced() to use only obs 3 in both inner means

balanced(fisher_mean)(x, w1, w2, na.rm = TRUE)
}