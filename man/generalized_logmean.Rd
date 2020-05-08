\name{generalized logarithmic mean}
\alias{generalized_logmean}
\alias{logmean}

\title{Generalized logarithmic mean}

\description{
Calculate a generalized logarithmic mean.
}

\usage{
generalized_logmean(a, b, r)

logmean(a, b)
}

\arguments{
\item{a, b}{A numeric vector.}

\item{r}{A number giving the order of the generalized logarithmic mean.}
}

\value{
A numeric value.
}

\details{
Returns the value of the generalized logarithmic mean of \code{a} and \code{b} of order \code{r}. See Bullen (2003, p. 385).

The usual definition of the generalized logarithmic mean lets \code{r} take on values from the extended real line, such that \code{r = -Inf/Inf} returns the minimum/maximum of \code{a} and \code{b}. This is not implemented here, as it does not seem to be of practical value.

As a matter of definition, both \code{a} and \code{b} should be strictly positive. This is not enforeced here, but the results may not make sense otherwise.
}

\references{
Bullen, P. S. (2003). Handbook of Means and Their Inequalities. Springer.
}

\examples{
## A useful identity

logmean(1:10, 10:1) * log(1:10 / 10:1)  - (1:10 - 10:1) # should be the all zero
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}
