\name{generalized mean}
\alias{mean_generalized}
\alias{mean_arithmetic}
\alias{mean_geometric}
\alias{mean_harmonic}

\title{Generalized mean}

\description{
Calculate a generalized mean.
}

\usage{
mean_generalized(x, w, r, na.rm = FALSE, scale = TRUE)

mean_arithmetic(x, w, na.rm = FALSE, scale = TRUE)

mean_geometric(x, w, na.rm = FALSE, scale = TRUE)

mean_harmonic(x, w, na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{x}{A numeric or logical vector.}

\item{w}{An optional vector of numeric or logical weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
A numeric value.
}

\details{
Returns the value of the generalized mean of \code{x} and weights \code{w} with exponent \code{r} (i.e., the weighted mean of \code{x} to the power of \code{r}, all raised to the power of 1 / \code{r}). This is also called the power mean or Holder mean. See Bullen (2003, p. 175) for a precise statement.

The underlying calculation is done with \code{mean_arithmetic}, which is mostly identical to \code{\link[stats]{weighted.mean}}. Other than doing more checks on input values and often being faster, missing values in the weights are not treated differently than missing values in \code{x}. Setting \code{na.rm = TRUE} drops missing values in both \code{x} and \code{w}, not just \code{x}.

The functions \code{mean_geometric} and \code{mean_harmonic} are probably the two most useful generalized means, other than \code{mean_arithmetic}, and correspond to setting \code{r = 0} and \code{r = -1} in \code{mean_generalized}.

The usual definition of the generalized mean lets \code{r} take on values from the extended real line, such that \code{r = -Inf/Inf} returns the minimum/maximum of \code{x}. This is not implemented here, as it does not seem to be of practical value.

For the geometric and harmonic means in particular, the implementation and performance is similar to the \code{geometric.mean} and \code{harmonic.mean} functions in the \code{psych} package, the \code{geomean} function in the FSA package, the \code{GMean} and \code{HMean} functions in the \code{DescTools} package, and the \code{geoMean} function in the \code{EnvStats} package, with the exception that weights are allowed.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense otherwise. Similarly, the weights should almost always be scaled to sum to 1.
}

\references{
Bullen, P. S. (2003). Handbook of Means and Their Inequalities. Springer.
}

\seealso{
\code{\link[gpindex]{logmean_generalized}} for the generalized logarithmic mean. 

\code{\link[gpindex]{weights_change}} can be used to turn an r-generalized mean into a k-generalized mean. 

\code{\link[gpindex]{weights_factor}} can be used to factor the weights to a turn a mean of products into a product of means.
}

\examples{
# Geometric mean

mean_geometric(2:3)

# Same as manual calculation

sqrt(2 * 3)

# Using prod to manually calculate the geometric mean can give misleading results

prod(1:1000)^(1/1000)
mean_geometric(1:1000)

# mean_geometric is usually a bit faster anyways

x <- runif(1000000)
system.time(prod(x^(1/length(x)))) - system.time(mean_geometric(x))

# Harmonic mean

mean_harmonic(2:3)

# Same as manual calculation

(sum((2:3)^(-1)) / 2)^(-1)

# Quadratic mean

mean_generalized(2:3, r = 2)

# Root mean square

sqrt(mean_generalized(2:3, r = 2))

}
