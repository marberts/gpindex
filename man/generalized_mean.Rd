\name{generalized mean}
\alias{generalized_mean}
\alias{arithmetic_mean}
\alias{geometric_mean}
\alias{harmonic_mean}

\title{Generalized mean}

\description{
Calculate a generalized mean.
}

\usage{
generalized_mean(x, w, r, na.rm = FALSE, scale = TRUE)

arithmetic_mean(x, w, na.rm = FALSE, scale = TRUE)

geometric_mean(x, w, na.rm = FALSE, scale = TRUE)

harmonic_mean(x, w, na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean.}

\item{na.rm}{Should missing values be removed?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
A numeric value.
}

\details{
Returns the value of the generalized mean of \code{x} and weights \code{w} with exponent \code{r} (i.e., the weighted mean of \code{x} to the power of \code{r}, all raised to the power of 1 / \code{r}). This is also called the power mean or Holder mean. See Bullen (2003, p. 175) for a precise statement.

The underlying calculation is done with \code{arithmetic_mean}, which is almost identical to \code{\link[stats]{weighted.mean}}. Other than doing more checks on input values and being considerably faster in some cases with missing values, setting \code{na.rm = TRUE} drops missing values in both \code{x} and \code{w}, not just \code{x}.

The usual definition of the generalized mean lets \code{r} take on values from the extended real line, such that \code{r = -Inf/Inf} returns the minimum/maximum of \code{x}. This is not implemented here, as it does not seem to be of practical value.

For the geometric and harmonic means in particular, the implementation and performance is similar to the \code{geometric.mean} and \code{harmonic.mean} functions in the \code{psych} package, the \code{geomean} function in the FSA package, the \code{GMean} and \code{HMean} functions in the \code{DescTools} package, and the \code{geoMean} function in the \code{EnvStats} package, with the exception that weights are allowed.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforeced here, but the results may not make sense otherwise. Similarly, the weights should almost always be scaled to sum to 1.
}

\references{
Bullen, P. S. (2003). Handbook of Means and Their Inequalities. Springer.
}

\seealso{
See \code{\link[gpindex]{generalized_logmean}} for the generalized logarithmic mean. \code{\link[gpindex]{change_weights}} can be used to turn an r-generalized mean into a k-generalized mean.
}

\examples{
## Geometric mean

geometric_mean(2:3)

# Same as manual calculation

sqrt(6)

# Using prod to manually calculate the geometric mean can give misleading results

prod(1:1000)^(1/1000)
geometric_mean(1:1000)

# geometric_mean is usuallya bit faster anyways

x <- runif(1000000)
system.time(prod(x^(1/length(x)))) - system.time(geometric_mean(x))

## Harmonic mean

harmonic_mean(2:3)

# Same as manual calculation

(sum((2:3)^(-1)) / 2)^(-1)

## Quadratic mean

generalized_mean(2:3, r = 2)

# Root mean square

sqrt(generalized_mean(2:3, r = 2))

}
