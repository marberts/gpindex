\name{nested mean}
\alias{mean_nested}
\alias{mean_fisher}

\title{Nested generalized means}

\description{
Calculate a mean of generalized means.
}

\usage{
mean_nested(r, s)

mean_fisher(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)), na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the outer generalized mean.}

\item{s}{A pair of finite numbers giving the order of the generalized means for the inner means in a nested generalized mean.}

\item{x}{A strictly positive numeric vector.}

\item{w1, w2}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed? By default missing values in \code{x} or \code{w} return a missing value.}
}

\value{
\code{mean_nested()} returns a function:

\code{function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)), na.rm = FALSE)}.

This computes the (unweighted) generalized mean of order \code{r} of the generalized mean of order \code{s[1]} of \code{x} with weights \code{w1} and the generalized mean of order \code{s[2]} of \code{x} with weights \code{w2}.

\code{mean_fisher()} returns a numeric value for the case of \code{r = 0} and \code{s = c(1, -1)}.
}

\seealso{
\code{\link{mean_generalized}} for the generalized mean.
}

\examples{
# Make some data

x <- 1:3
w <- c(0.25, 0.25, 0.5)

# A function to make the superlative quadratic mean price index in chapter 17, 
# section B.5.1, of the PPI manual as a product of generalized means

quadratic_mean_index <- function(x, w0, w1, r) {
  x <- sqrt(x) 
  mean_generalized(r)(x, w0) * mean_generalized(-r)(x, w1)
}

quadratic_mean_index(1:3, 4:6, 7:9, 2)

# Same as the geometric mean of two generalized means (with the order halved)

quadratic_mean_index2 <- function(r) mean_nested(0, c(r / 2, -r / 2))

quadratic_mean_index2(2)(1:3, 4:6, 7:9)
}