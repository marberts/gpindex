\name{change weights}
\alias{change_weights}
\alias{geometric_to_arithmetic}
\alias{harmonic_to_arithmetic}
\alias{arithmetic_to_geometric}
\alias{harmonic_to_geometric}

\title{Change the weights in a generalized mean}

\description{
Calculate the weights to turn a \code{r}-generalized mean into a \code{k}-generalized mean
}

\usage{
change_weights(x, w, r, k, na.rm = FALSE)

geometric_to_arithmetic(x, w, na.rm = FALSE)

harmonic_to_arithmetic(x, w, na.rm = FALSE)

arithmetic_to_geometric(x, w, na.rm = FALSE)

harmonic_to_geometric(x, w, na.rm = FALSE)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean.}

\item{k}{A number giving the exponent of the generalized mean.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{x}.
}

\details{
Returns a vector of weights \code{v} such that \code{generalized_mean(x, w, r) = generalized_mean(x, v, k)}. This generalizes the result in section 4.x of Balk (2008) when \code{r} and \code{k} are 0 or 1.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforeced here, but the results may not make sense.
}

\references{
Balk, B. M. (2008). Price and Quantity Indices. Cambridge University Press.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
x <- 1:10
geometric_mean(x)

# Calculate the geometric mean as an arithmetic and harmonic by changing the weights
arithmetic_mean(x, geometric_to_arithmetic(x))
harmonic_mean(x, change_weights(x, r = 0, k = -1))

}
