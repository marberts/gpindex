\name{generalized logarithmic mean}
\alias{logmean_generalized}
\alias{logmean}
\alias{mean_extended}

\title{Generalized logarithmic mean / extended mean}

\description{
Calculate a generalized logarithmic mean / extended mean.
}

\usage{
logmean_generalized(r)

mean_extended(r, s)

logmean(a, b, tol = .Machine$double.eps^0.5)
}

\arguments{
\item{r, s}{A finite number giving the order of the generalized logarithmic mean / extended mean.}

\item{a, b}{A numeric vector.}

\item{tol}{The tolerance used to determine if \code{a == b}.}
}

\value{
\code{logmean_generalized()} and \code{mean_extended()} return a function: \code{function(a, b, tol)}. 

\code{logmean()} returns a numeric vector the same length as \code{max(length(a), length(b))}.
}

\details{
The function \code{mean_extended()} returns a function to compute the extended mean of \code{a} and \code{b} of order \code{r} and \code{s}. See Bullen (2003, p. 393) for a precise statement. This is also called the difference mean, Stolarsky mean, or extended mean-value mean. 

The function \code{logmean_generalized()} returns a function to compute the generalized logarithmic mean of \code{a} and \code{b} of order \code{r}. See Bullen (2003, p. 385) for a precise statement, or \url{https://en.wikipedia.org/wiki/Stolarsky_mean}. The generalized logarithmic mean is a special case of the extended mean, setting \code{s = 1} in \code{mean_extended()}, but is more commonly used for price indexes.

The function \code{logmean()} returns the ordinary logarithmic mean, the most useful generalized logarithmic mean, and corresponds to \code{logmean_generalized(1)}.

As a matter of definition, both \code{a} and \code{b} should be strictly positive. This is not enforced here, but the results may not make sense when the generalized logarithmic mean / extended mean is not defined.

By definition, the generalized logarithmic mean / extended mean of \code{a} and \code{b} is \code{a} when \code{a == b}. The \code{tol} argument is used to test equality by checking if \code{abs(a - b) <= tol}. The default value is the same as in \code{\link[base:all.equal]{all.equal()}}. Setting \code{tol = 0} will test for exact equality, but can give misleading results in certain applications when \code{a} and \code{b} are computed values.
}

\section{Warning}{
Passing very small values for \code{r} or \code{s} can give misleading results, and warning is given whenever \code{abs(r)} or \code{abs(s)} is sufficiently small. Similarly, values for \code{r} and \code{s} that are very close in value, but not equal, can give misleading results. In general, \code{r} and \code{s} should not be computed values.
}

\note{
\code{logmean_generalized()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[base:pmin]{pmin()}}/\code{\link[base:pmax]{pmax()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer.

Stolarsky, K. B. (1975). Generalization of the Logartihmic Mean. \emph{Mathematics Magazine}, 48(2):87-92.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{weights_transmute}} uses the extended mean to turn an r-generalized mean into an s-generalized mean.
}

\examples{
# The arithmetic and geometric means are special cases 
# of the generalized logarithmic mean

x <- 8:5
y <- 1:4
all.equal(logmean_generalized(2)(x, y), (x + y) / 2)
all.equal(logmean_generalized(-1)(x, y), sqrt(x * y))

# The loarithmic mean lies between the arithmetic and geometric means

all(logmean(x, y) < (x + y) / 2) & all(logmean(x, y) > sqrt(x * y))

# The harmonic mean cannot be expressed as a logarithmic mean, 
# but can be expressed as an extended mean

all.equal(mean_extended(-2, -1)(x, y), 2 / (1 / x + 1 / y))

# A useful identity

all.equal(logmean(x, y) * log(x / y), x - y) 

# Works for other orders, too

r <- 2

all.equal(logmean_generalized(r)(x, y) * (r * (x - y))^(1 / (r - 1)), 
          (x^r - y^r)^(1 / (r - 1)))

# Some other identities from Bullen (example iii on page 385)

all.equal(logmean_generalized(-2)(1, 2), 
          (mean_harmonic(1:2) * mean_geometric(1:2)^2)^(1 / 3))

all.equal(logmean_generalized(0.5)(1, 2), 
          (mean_arithmetic(1:2) + mean_geometric(1:2)) / 2)

all.equal(logmean(1, 2),
          mean_geometric(1:2)^2 * logmean(1, 1/2))
          
# Logarithmic means can be represented as integrals

logmean(2, 3)
stats::integrate(function(t) 2^(1 - t) * 3^t, 0, 1)
}