\name{generalized_mean}
\alias{generalized_mean}
\alias{arithmetic_mean}
\alias{geometric_mean}
\alias{harmonic_mean}

\title{Generalized mean}

\description{
Calculate a generalized mean.
}

\usage{
## Function factory
generalized_mean(r)

## Special cases
arithmetic_mean(x, w, na.rm = FALSE)

geometric_mean(x, w, na.rm = FALSE)

harmonic_mean(x, w, na.rm = FALSE)
}

\arguments{
\item{r}{A finite number giving the order of the generalized mean.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed? By default missing values in \code{x} or \code{w} return a missing value.}
}

\details{
The function \code{generalized_mean()} returns a function to compute the generalized mean of \code{x} with weights \code{w} and exponent \code{r} (i.e., \eqn{\prod_{i = 1}^{n} x_{i}^{w_{i}}}{\prod x^w} when \eqn{r = 0}{r = 0} and \eqn{\left(\sum_{i = 1}^{n} w_{i} x_{i}^{r}\right)^{1 / r}}{(\sum wx^r)^1/r} otherwise). This is also called the power mean, Holder mean, or \eqn{l_p}{l_p} mean. See Bullen (2003, p. 175) for a definition, or \url{https://en.wikipedia.org/wiki/Generalized_mean}. The generalized mean is the solution to the optimal prediction problem: choose \eqn{m}{m} to minimize \eqn{\sum_{i = 1}^{n} w_{i} \left[\log(x_{i}) - \log(m) \right]^2}{\sum w [log(x) - log(m)]^2} when \eqn{r = 0}{r = 0}, \eqn{\sum_{i = 1}^{n} w_{i} \left[x_{i}^r - m^r \right]^2}{\sum w [x^r - m^r]^2} otherwise.

The functions \code{arithmetic_mean()}, \code{geometric_mean()}, and \code{harmonic_mean()} compute the arithmetic, geometric, and harmonic (or subcontrary) means, also known as the Pythagorean means. These are the most useful means for making price indexes, and correspond to setting \code{r = 1}, \code{r = 0}, and \code{r = -1} in \code{generalized_mean()}. 

Both \code{x} and \code{w} should be strictly positive (and finite), especially for the purpose of making a price index. This is not enforced, but the results may not make sense if the generalized mean is not defined. There are two exceptions to this.
\enumerate{
\item{The convention in Hardy et al. (1952, p. 13) is used in cases where \code{x} has zeros: the generalized mean is 0 whenever \code{w} is strictly positive and \code{r} < 0. (The analogous convention holds whenever at least one element of \code{x} is \code{Inf}: the generalized mean is \code{Inf} whenever \code{w} is strictly positive and \code{r > 0}.)} 

\item{Some authors let \code{w} be non-negative and sum to 1 (e.g., Sydsaeter et al., 2005, p. 47). If \code{w} has zeros, then the corresponding element of \code{x} has no impact on the mean whenever \code{x} is strictly positive. Unlike \code{\link[=weighted.mean]{weighted.mean()}}, however, zeros in \code{w} are not strong zeros, so infinite values in \code{x} will propagate even if the corresponding elements of \code{w} are zero.}
}

The weights are scaled to sum to 1 to satisfy the definition of a generalized mean. There are certain price indexes where the weights should not be scaled (e.g., the Vartia-I index); use \code{\link[=sum]{sum()}} for these cases.

The underlying calculation returned by \code{generalized_mean()} is mostly identical to \code{\link[=weighted.mean]{weighted.mean()}}, with one important exception: missing values in the weights are not treated differently than missing values in \code{x}. Setting \code{na.rm = TRUE} drops missing values in both \code{x} and \code{w}, not just \code{x}. This ensures that certain useful identities are satisfied with missing values in \code{x}. In most cases \code{arithmetic_mean()} is a drop-in replacement for \code{\link[=weighted.mean]{weighted.mean()}}.
}

\value{
\code{generalized_mean()} returns a function:

\preformatted{function(x, w, na.rm = FALSE){...}}

This computes the generalized mean of order \code{r} of \code{x} with weights \code{w}.

\code{arithmetic_mean()}, \code{geometric_mean()}, and \code{harmonic_mean()} each return a numeric value for the generalized means of order 1, 0, and -1.
}

\section{Warning}{
Passing very small values for \code{r} can give misleading results, and warning is given whenever \code{abs(r)} is sufficiently small. In general, \code{r} should not be a computed value.
}

\note{
\code{generalized_mean()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[=min]{min()}}/\code{\link[=max]{max()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite.

There are a number of existing functions for calculating \emph{unweighted} geometric and harmonic means, namely the \code{geometric.mean()} and \code{harmonic.mean()} functions in the \pkg{psych} package, the \code{geomean()} function in the \pkg{FSA} package, the \code{GMean()} and \code{HMean()} functions in the \pkg{DescTools} package, and the \code{geoMean()} function in the \pkg{EnvStats} package. Similarly, the \code{ci_generalized_mean()} function in the \pkg{Compind} package calculates an \emph{unweighted} generalized mean.
}

\references{
Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer Science+Business Media.

Fisher, I. (1922). \emph{The Making of Index Numbers}. Houghton Mifflin Company.

Hardy, G., Littlewood, J. E., and Polya, G. (1952). \emph{Inequalities} (2nd edition). Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

Lord, N. (2002). Does Smaller Spread Always Mean Larger Product? \emph{The Mathematical Gazette}, 86(506): 273-274.

Sydsaeter, K., Strom, A., and Berck, P. (2005). \emph{Economists' Mathematical Manual} (4th edition). Springer.
}

\seealso{
\code{\link{generalized_logmean}} for the generalized logarithmic mean.

\code{\link{lehmer_mean}} for the Lehmer mean, an alternative to the generalized mean.

\code{\link{nested_mean}} for nesting generalized means.

\code{\link{transmute_weights}} transforms the weights to turn a generalized mean of order \eqn{r}{r} into a generalized mean of order \eqn{s}{s}. 

\code{\link{factor_weights}} calculates the weights to factor a mean of products into a product of means.

\code{\link{price_index}} and \code{\link{quantity_index}} for simple wrappers that use \code{generalized_mean()} to calculate common indexes.

\code{\link{back_period}}/\code{\link{base_period}} for a simple utility function to turn prices in a table into price relatives.
}

\examples{
x <- 1:3
w <- c(0.25, 0.25, 0.5)

#---- Common generalized means ----

# Arithmetic mean

arithmetic_mean(x, w) # same as weighted.mean(x, w)

# Geometric mean

geometric_mean(x, w) # same as prod(x^w)

# Using prod() to manually calculate the geometric mean 
# can give misleading results

z <- 1:1000
prod(z)^(1 / length(z)) # overflow
geometric_mean(z)

z <- seq(0.0001, by = 0.0005, length.out = 1000)
prod(z)^(1 / length(z)) # underflow
geometric_mean(z)

# Harmonic mean

harmonic_mean(x, w) # same as 1 / weighted.mean(1 / x, w)

# Quadratic mean / root mean square

generalized_mean(2)(x, w)

# Cubic mean
# Notice that this is larger than the other means so far because 
# the generalized mean is increasing in r

generalized_mean(3)(x, w)

#---- Comparing the Pythagorean means ----

# The dispersion between the arithmetic, geometric, and harmonic
# mean usually increases as the variance of 'x' increases

x <- c(1, 3, 5)
y <- c(2, 3, 4)

var(x) > var(y)

arithmetic_mean(x) - geometric_mean(x)
arithmetic_mean(y) - geometric_mean(y)

geometric_mean(x) - harmonic_mean(x)
geometric_mean(y) - harmonic_mean(y)

# But the dispersion between these means is only bounded by
# the variance (Bullen, 2003, p. 156)

arithmetic_mean(x) - geometric_mean(x) >= 2 / 3 * var(x) / (2 * max(x))
arithmetic_mean(x) - geometric_mean(x) <= 2 / 3 * var(x) / (2 * min(x))

# Example by Lord (2002) where the dispersion decreases as 
# the variance increases, counter to the claims by 
# Fisher (1922, p. 108) and the CPI manual (par. 1.14)

x <- (5 + c(sqrt(5), -sqrt(5), -3)) / 4
y <- (16 + c(7 * sqrt(2), -7 * sqrt(2), 0)) / 16

var(x) > var(y)

arithmetic_mean(x) - geometric_mean(x)
arithmetic_mean(y) - geometric_mean(y)

geometric_mean(x) - harmonic_mean(x)
geometric_mean(y) - harmonic_mean(y)

# The "bias" in the arithmetic and harmonic indexes is also 
# smaller in this case, counter to the claim 
# by Fisher (1922, p. 108)

arithmetic_mean(x) * arithmetic_mean(1 / x) - 1
arithmetic_mean(y) * arithmetic_mean(1 / y) - 1

harmonic_mean(x) * harmonic_mean(1 / x) - 1
harmonic_mean(y) * harmonic_mean(1 / y) - 1

#---- Missing values ----

w[2] <- NA

arithmetic_mean(x, w)

arithmetic_mean(x, w, na.rm = TRUE) # drop the second observation
weighted.mean(x, w, na.rm = TRUE) # still returns NA
}
