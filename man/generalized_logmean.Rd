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
A numeric vector, the same length as \code{max(length(a), length(b))}.
}

\details{
The function \code{logmean_generalized()} returns the value of the generalized logarithmic mean of \code{a} and \code{b} of order \code{r}. See Bullen (2003, pp. 385, 393) for a precise statement, or \url{https://en.wikipedia.org/wiki/Stolarsky_mean}. 

The function \code{logmean()} returns the ordinary logarithmic mean, the most useful generalized logarithmic mean, and corresponds to setting \code{r = 0} in \code{logmean_generalized()}.

As a matter of definition, both \code{a} and \code{b} should be strictly positive. This is not enforced here, but the results may not make sense when the generalized logarithmic mean is not defined.

By definition, the generalized logarithmic mean of \code{a} and \code{b} is \code{a} when \code{a == b}. The \code{tol} argument is used to test equality by checking if \code{abs(a - b) <= tol}. The default value is the same as in \code{\link[base:all.equal]{all.equal()}}. Setting \code{tol = 0} will test for exact equality, but can give misleading results in certain applications when \code{a} and \code{b} are computed values.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{weights_change}} uses the generalized logarithmic mean to turn an r-generalized mean into a k-generalized mean.
}

\examples{
# The arithmetic and geometric means are special cases of the generalized logarithmic mean

x <- 8:5
y <- 1:4
all.equal(logmean_generalized(x, y, 2), (x + y) / 2)
all.equal(logmean_generalized(x, y, -1), sqrt(x * y))

# A useful identity

all.equal(logmean(x, y) * log(x / y), x - y) 

# Works for other orders, too

r <- 2

all.equal(logmean_generalized(x, y, r) * (r * (x - y))^(1 / (r - 1)), 
          (x^r - y^r)^(1 / (r - 1)))

# Some other identities from Bullen (example iii on page 385)

all.equal(logmean_generalized(1, 2, -2), 
          (mean_harmonic(1:2) * mean_geometric(1:2)^2)^(1/3))

all.equal(logmean_generalized(1, 2, 0.5), 
          (mean_arithmetic(1:2) + mean_geometric(1:2)) / 2)

all.equal(logmean(1, 2),
          mean_geometric(1:2)^2 * logmean(1, 1/2))
}