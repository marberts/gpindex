\name{change weights}
\alias{change_weights}
\alias{geometric_to_arithmetic}
\alias{harmonic_to_arithmetic}
\alias{arithmetic_to_geometric}
\alias{harmonic_to_geometric}

\title{Change the weights in a generalized mean}

\description{
Calculate the weights to turn an r-generalized mean into a k-generalized mean.
}

\usage{
change_weights(x, w, r, k, na.rm = FALSE, M)

geometric_to_arithmetic(x, w, na.rm = FALSE, M)

harmonic_to_arithmetic(x, w, na.rm = FALSE, M)

arithmetic_to_geometric(x, w, na.rm = FALSE, M)

harmonic_to_geometric(x, w, na.rm = FALSE, M)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean. See details.}

\item{k}{A number giving the exponent of the generalized mean. See details.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex]{generalized_mean}}?}

\item{M}{The value of the r-generalized mean, if known.}
}

\value{
A numeric vector, the same length as \code{w}.
}

\details{
Returns a vector of weights \code{v} such that \code{generalized_mean(x, w, r) = generalized_mean(x, v, k)}. This generalizes the result in section 4.2 of Balk (2008) when \code{r} and \code{k} are 0 or 1.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforeced here, but the results may not make sense otherwise.

In general the weights depend on the value of \code{generalized_mean(x, w, r)}. In many cases this value is known prior to calling the function, and can be supplied to save some computations. Otherwise, it will be calculated from the values of \code{x} and \code{w}.

As the return value is the same length as \code{w}, any NAs in \code{x} or \code{w} will return NA. Setting \code{na.rm = TRUE} simply sets \code{na.rm = TRUE} in the call to \code{\link[gpindex]{generalized_mean}}.
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
x <- 1:10
geometric_mean(x)

# Calculate the geometric mean as an arithmetic mean and harmonic mean by changing the weights

arithmetic_mean(x, geometric_to_arithmetic(x))
harmonic_mean(x, change_weights(x, r = 0, k = -1))

}
