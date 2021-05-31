\name{nested mean}
\alias{mean_nested}
\alias{mean_fisher}

\title{Nested generalized mean}

\description{
Calculate the (outer) generalized mean of two (inner) generalized mean.
}

\usage{
mean_nested(r, s, t)

mean_fisher(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)), na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the outer generalized mean.}

\item{s}{A pair of finite numbers giving the order of the inner generalized means.}

\item{t}{A pair of strictly positive weights for the inner generalized means.}

\item{x}{A strictly positive numeric vector.}

\item{w1, w2}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x}, \code{w1}, and \code{w2} be removed? By default missing values in \code{x}, \code{w1}, or \code{w2} return a missing value.}
}

\value{
\code{mean_nested()} returns a function:

\code{function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)), na.rm = FALSE)}.

This computes the generalized mean of order \code{r} of the generalized mean of order \code{s[1]} of \code{x} with weights \code{w1} and the generalized mean of order \code{s[2]} of \code{x} with weights \code{w2}.

\code{mean_fisher()} returns a numeric value (the geometric mean of the arithmetic and harmonic means).
}

\note{
There is some ambiguity about how to remove missing values in \code{w1} or \code{w2} when \code{na.rm = TRUE}. The approach here is to remove missing values when calculating each of the inner means individually, rather than removing all missing values prior to any calculations. This means that a different number of data points could be used to calculate the inner means.
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.

Lent, J. and Dorfman, A, H. (2009). Using a Weighted Average of Base Period Price Indexes to Approximate a Superlative Index. \emph{Journal of Official Statistics}, 25(1):139--149.
}

\seealso{
\code{\link{mean_generalized}} for the generalized mean.

\code{\link{contributions}} for quote contributions for indexes based on nested generalized means, like the Fisher index.
}

\examples{
x <- 1:3
w1 <- 4:6
w2 <- 7:9

#---- Making superlative indexes ----

# A function to make the superlative quadratic mean price index in chapter 17, 
# section B.5.1, of the PPI manual as a product of generalized means

quadratic_mean_index <- function(x, w0, w1, r) {
  x <- sqrt(x) 
  mean_generalized(r)(x, w0) * mean_generalized(-r)(x, w1)
}

quadratic_mean_index(x, w1, w2, 2)

# Same as the nested generalized mean (with the order halved)

quadratic_mean_index2 <- function(r) mean_nested(0, c(r / 2, -r / 2), c(0.5, 0.5))

quadratic_mean_index2(2)(x, w1, w2)

# The arithmetic AG mean index by Lent and Dorfman (2009)

ag_mean_index <- function(tau) mean_nested(1, c(0, 1), c(tau, 1 - tau))

ag_mean_index(0.25)(x, w1, w1)

#---- Missing values ----

x[1] <- NA
w1[2] <- NA

mean_fisher(x, w1, w2, na.rm = TRUE)

# Same as using obs 2 and 3 in an arithmetic mean, and obs 3 in a harmonic mean

mean_geometric(c(mean_arithmetic(x, w1, na.rm = TRUE), mean_harmonic(x, w2, na.rm = TRUE)))

# Use complete.cases to use only obs 3 in both inner means

keep <- complete.cases(x, w1, w2)

mean_fisher(x[keep], w1[keep], w2[keep])
}