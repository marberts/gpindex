\name{change weights}
\alias{weights_change}
\alias{weights_g2a}
\alias{weights_h2a}
\alias{weights_a2g}
\alias{weights_h2g}

\title{Change the weights in a generalized mean}

\description{
Calculate the weights to turn an r-generalized mean into an s-generalized mean.
}

\usage{
weights_change(r, s)

weights_g2a(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

weights_h2a(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

weights_a2g(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

weights_h2g(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{r}{A number giving the exponent of the generalized mean associated with \code{w}. See details.}

\item{s}{A number giving the exponent of the generalized mean for which weights are desired. See details.}

\item{x}{A numeric vector.}

\item{w}{A numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex:mean_generalized]{mean_generalized()}} and \code{\link[gpindex:weights_scale]{weights_scale()}}?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
\code{weights_change()} returns a function: \code{function(x, w, na.rm, scale)}; otherwise, a numeric vector the same length as \code{x}.
}

\details{
The function \code{weights_change()} returns a function to compute a vector of weights \code{v(x, w)} such that

\code{mean_generalized(r)(x, w) == mean_generalized(s)(x, v(x, w))}. 

These weights are calculated as 

\code{w * mean_extended(r, s)(x, mean_generalized(r)(x, w))^(r - s)}.

This generalizes the result in section 4.2 of Balk (2008) when \code{r} and \code{s} are 0 or 1, although these are usually the most important cases. 

The functions \code{weights_g2a()} and \code{weights_h2a()} calculate the weights to turn a geometric and harmonic mean into an arithmetic mean (i.e., setting \code{r = 0} and \code{r = -1} when \code{s = 1} in \code{weights_change()}). The functions \code{weights_a2g()} and \code{weights_h2g()} are similarly for turning an arithmetic and harmonic mean into a geometric mean (i.e., setting \code{r = 1} and \code{r = -1} when \code{s = 0} in \code{weights_change()}).

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense in cases where the generalized mean and generalized logarithmic mean are not defined.

As the return value is the same length as \code{w}, any NAs in \code{x} or \code{w} will return NA. Setting \code{na.rm = TRUE} simply sets \code{na.rm = TRUE} in the call to \code{\link[gpindex:mean_generalized]{mean_generalized()}}, and \code{\link[gpindex:weights_scale]{weights_scale()}} if \code{scale = TRUE}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{logmean_generalized}} for the generalized logarithmic mean. 

\code{\link[gpindex]{weights_scale}} to scale the weights to sum to 1.
}

\examples{
# Calculate the geometric mean as an arithmetic mean and harmonic mean by changing the weights

x <- 1:10
mean_geometric(x)
mean_arithmetic(x, weights_g2a(x))
mean_harmonic(x, weights_change(0, -1)(x))

# Works for nested means, too

w1 <- runif(10)
w2 <- runif(10)

mean_geometric(c(mean_arithmetic(x, w1), mean_harmonic(x, w2)))

v0 <- weights_g2a(c(mean_arithmetic(x, w1), mean_harmonic(x, w2)))
v1 <- weights_scale(w1)
v2 <- weights_h2a(x, w2)
mean_arithmetic(x, v0[1] * v1 + v0[2] * v2)
}