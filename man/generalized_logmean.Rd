\name{generalized logarithmic mean}
\alias{logmean_generalized}
\alias{logmean}

\title{Generalized logarithmic mean}

\description{
Calculate a generalized logarithmic mean.
}

\usage{
logmean_generalized(a, b, r, tol = .Machine$double.eps^0.5)

logmean(a, b, tol = .Machine$double.eps^0.5)
}

\arguments{
\item{a, b}{A numeric vector.}

\item{r}{A number giving the order of the generalized logarithmic mean.}

\item{tol}{The tolerance used to determine if \code{a == b}.}
}

\value{
A numeric vector, the same length as \code{max(length(a), length(b)}.
}

\details{
Returns the value of the generalized logarithmic mean of \code{a} and \code{b} of order \code{r}. See Bullen (2003, p. 385) for a precise statement. The logarithmic mean corresponds to the generalized logarithmic mean of order 0. The function \code{logmean} simply calls \code{logmean_generalized} with \code{r = 0}.

The usual definition of the generalized logarithmic mean lets \code{r} take on values from the extended real line, such that \code{r = -Inf/Inf} returns the minimum/maximum of \code{a} and \code{b}. This is not implemented here, as it does not seem to be of practical value.

As a matter of definition, both \code{a} and \code{b} should be strictly positive. This is not enforeced here, but the results may not make sense otherwise.
}

\references{
Bullen, P. S. (2003). Handbook of Means and Their Inequalities. Springer.
}

\seealso{
See \code{\link[gpindex]{mean_generalized}} for the generalized mean. The generalized logarithmic means is used in \code{\link[gpindex]{weights_change}} to turn an r-generalized mean into a k-generalized mean.
}

\examples{
## A useful identity (should be all zero)

x <- 8:5
y <- 4:1
logmean(x, y) * log(x / y)  - (x - y) 

# Works for other orders, too

r <- 2

logmean_generalized(x, y, r) * (r * (x - y))^(1 / (r - 1)) - (x^r - y^r)^(1 / (r - 1))
}

