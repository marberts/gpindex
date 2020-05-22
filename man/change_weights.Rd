\name{change weights}
\alias{weights_change}
\alias{weights_g2a}
\alias{weights_h2a}
\alias{weights_a2g}
\alias{weights_h2g}

\title{Change the weights in a generalized mean}

\description{
Calculate the weights to turn an r-generalized mean into a k-generalized mean.
}

\usage{
weights_change(x, w, r, k, na.rm = FALSE, M)

weights_g2a(x, w, na.rm = FALSE, M)

weights_h2a(x, w, na.rm = FALSE, M)

weights_a2g(x, w, na.rm = FALSE, M)

weights_h2g(x, w, na.rm = FALSE, M)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean. See details.}

\item{k}{A number giving the exponent of the generalized mean. See details.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex]{mean_generalized}}?}

\item{M}{The value of the r-generalized mean, if known.}
}

\value{
A numeric vector, the same length as \code{w}.
}

\details{
Returns a vector of weights \code{v} such that \code{mean_generalized(x, w, r) = mean_generalized(x, v, k)}. These weights are calculated as \code{w * logmean_generalized(x, mean_generalized(x, w, r), r)^(r - 1) / logmean_generalized(x, mean_generalized(x, w, r), k)^(k - 1)}. This generalizes the result in section 4.2 of Balk (2008) when \code{r} and \code{k} are 0 or 1, although these are usually the most important cases. The functions \code{weights_g2a} and \code{weights_h2a} calculate the weights to turn a geometric and harmonic average into an arithmetic average (i.e., setting \code{r = 0} and \code{r = -1} when \code{k = 1} in \code{weights_change}). The functions \code{weights_a2g} and \code{weights_h2g} are similarly for turning an arithmetic and harmonic average into a geometric average.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense otherwise.

The weights depend on the value of \code{mean_generalized(x, w, r)}. In many cases this value is known prior to calling the function, and can be supplied to save some computations. Otherwise, it will be calculated from the values of \code{x} and \code{w}.

As the return value is the same length as \code{w}, any NAs in \code{x} or \code{w} will return NA. Setting \code{na.rm = TRUE} simply sets \code{na.rm = TRUE} in the call to \code{\link[gpindex]{mean_generalized}}.
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{logmean_generalized}} for the generalized logarithmic mean. 
}

\examples{
# The geometric mean

x <- 1:10
mean_geometric(x)

# Calculate the geometric mean as an arithmetic mean and harmonic mean by changing the weights

mean_arithmetic(x, weights_g2a(x))
mean_harmonic(x, weights_change(x, r = 0, k = -1))

}
