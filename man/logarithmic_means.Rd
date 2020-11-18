\name{logarithmic means}
\alias{logmean_generalized}
\alias{logmean}
\alias{mean_extended}

\title{Logarithmic means}

\description{
Calculate a generalized logarithmic mean / extended mean.
}

\usage{
mean_extended(r, s)

logmean_generalized(r)

logmean(a, b, tol = .Machine$double.eps^0.5)
}

\arguments{
\item{r, s}{A finite number giving the order of the generalized logarithmic mean / extended mean.}

\item{a, b}{A strictly positive numeric vector.}

\item{tol}{The tolerance used to determine if \code{a == b}.}
}

\value{
\code{logmean_generalized()} and \code{mean_extended()} return a function: 

\code{function(a, b, tol = .Machine$double.eps^0.5)}. 

\code{logmean()} returns a numeric vector the same length as \code{max(length(a), length(b))}.
}

\details{
The function \code{mean_extended()} returns a function to compute the extended mean of \code{a} and \code{b} of orders \code{r} and \code{s}. See Bullen (2003, p. 393) for a definition. This is also called the difference mean, Stolarsky mean, or extended mean-value mean. 

The function \code{logmean_generalized()} returns a function to compute the generalized logarithmic mean of \code{a} and \code{b} of order \code{r}. See Bullen (2003, p. 385) for a definition, or \url{https://en.wikipedia.org/wiki/Stolarsky_mean}. The generalized logarithmic mean is a special case of the extended mean, corresponding to \code{mean_extended(r, 1)()}, but is more commonly used for price indexes.

The function \code{logmean()} returns the ordinary logarithmic mean, and corresponds to \code{logmean_generalized(1)()}.

Both \code{a} and \code{b} should be strictly positive. This is not enforced, but the results may not make sense when the generalized logarithmic mean / extended mean is not defined.

By definition, the generalized logarithmic mean / extended mean of \code{a} and \code{b} is \code{a} when \code{a == b}. The \code{tol} argument is used to test equality by checking if \code{abs(a - b) <= tol}. The default value is the same as in \code{\link[base:all.equal]{all.equal()}}. Setting \code{tol = 0} will test for exact equality, but can give misleading results when \code{a} and \code{b} are computed values.
}

\section{Warning}{
Passing very small values for \code{r} or \code{s} can give misleading results, and warning is given whenever \code{abs(r)} or \code{abs(s)} is sufficiently small. Similarly, values for \code{r} and \code{s} that are very close in value, but not equal, can give misleading results. In general, \code{r} and \code{s} should not be computed values.
}

\note{
\code{logmean_generalized()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[base:pmin]{pmin()}}/\code{\link[base:pmax]{pmax()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite as in the original formulation by Stolarsky (1975).
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer Science+Business Media.

Stolarsky, K. B. (1975). Generalizations of the Logarithmic Mean. \emph{Mathematics Magazine}, 48(2): 87-92.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{weights_transmute}} uses the extended mean to turn an r-generalized mean into an s-generalized mean.
}

\examples{
# Make some data

x <- 8:5
y <- 1:4

# The arithmetic and geometric means are special cases of the generalized 
# logarithmic mean

all.equal(logmean_generalized(2)(x, y), (x + y) / 2)
all.equal(logmean_generalized(-1)(x, y), sqrt(x * y))

# The logarithmic mean lies between the arithmetic and geometric means
# because the generalized logarithmic mean is increasing in r

all(logmean(x, y) < (x + y) / 2) & all(logmean(x, y) > sqrt(x * y))

# The harmonic mean cannot be expressed as a logarithmic mean, but can be 
# expressed as an extended mean

all.equal(mean_extended(-2, -1)(x, y), 2 / (1 / x + 1 / y))

# The quadratic mean is also a type of extended mean

all.equal(mean_extended(2, 4)(x, y), sqrt(x^2 / 2 + y^2 / 2))

# As are heronian and centroidal means

all.equal(mean_extended(0.5, 1.5)(x, y), (x + sqrt(x * y) + y) / 3)
all.equal(mean_extended(2, 3)(x, y), 2 / 3 * (x^2 + x * y + y^2) / (x + y))

#--------------------

# The logarithmic mean can be approximated as a convex combination of the 
# arithmetic and geometric means that gives more weight to the geometric mean

approx1 <- 1 / 3 * (x + y) / 2 + 2 / 3 * sqrt(x * y) 
approx2 <- ((x + y) / 2)^(1 / 3) * (sqrt(x * y))^(2 / 3) 

approx1 - logmean(x, y) # always a positive approximation error
approx2 - logmean(x, y) # a negative approximation error in this case

# A better approximation

correction <- (log(x / y) / pi)^4 / 32
approx1 / (1 + correction) - logmean(x, y)

#--------------------

# A useful identity for turning an additive change into a proportionate 
# change

all.equal(logmean(x, y) * log(x / y), x - y) 

# Works for other orders, too

r <- 2

all.equal(logmean_generalized(r)(x, y)^(r - 1) * (r * (x - y)), (x^r - y^r))

# Some other identities

all.equal(logmean_generalized(-2)(1, 2), 
          (mean_harmonic(1:2) * mean_geometric(1:2)^2)^(1 / 3))

all.equal(logmean_generalized(0.5)(1, 2), 
          (mean_arithmetic(1:2) + mean_geometric(1:2)) / 2)

all.equal(logmean(1, 2),
          mean_geometric(1:2)^2 * logmean(1, 1/2))

#--------------------
          
# Logarithmic means can be represented as integrals

logmean(2, 3)
stats::integrate(function(t) 2^(1 - t) * 3^t, 0, 1)$value
1 / stats::integrate(function(t) 1 / (2 * (1 - t) + 3 * t), 0, 1)$value
}