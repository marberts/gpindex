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
mean_generalized(r)

mean_arithmetic(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

mean_geometric(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

mean_harmonic(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{r}{A finite number giving the order of the generalized mean.}

\item{x}{A strictly positive numeric vector.}

\item{w}{A strictly positive numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
\code{mean_generalized()} returns a function:

\code{function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)}. 

\code{mean_arithmetic()}, \code{mean_geometric()}, and \code{mean_harmonic()} each return a numeric value.
}

\details{
The function \code{mean_generalized()} returns a function to compute the generalized mean of \code{x} with weights \code{w} and exponent \code{r} (i.e., \eqn{\prod_{i = 1}^{n} x_{i}^{w_{i}}}{\prod x^w} when \eqn{r = 0}{r = 0} and \eqn{\left(\sum_{i = 1}^{n} w_{i} x_{i}^{r}\right)^{1 / r}}{(\sum wx^r)^1/r} otherwise). This is also called the power mean, Holder mean, or \eqn{l_p}{l_p} mean. See Bullen (2003, p. 175) for a definition, or \url{https://en.wikipedia.org/wiki/Power_mean}. The generalized mean is the solution to the optimal prediction problem: choose \eqn{m}{m} to minimize  \eqn{\sum_{i = 1}^{n} w_{i} \left[\log(x_{i}) - \log(m) \right]^2}{\sum w [log(x) - log(m)]^2} when \eqn{r = 0}{r = 0}, \eqn{\sum_{i = 1}^{n} w_{i} \left[x_{i}^r - m^r \right]^2}{\sum w [x^r - m^r]^2} otherwise.

The functions \code{mean_arithmetic()}, \code{mean_geometric()}, and \code{mean_harmonic()} compute the arithmetic, geometric, and harmonic (or subcontrary) means, also known as the Pythagorean means. These are the most useful means for making price indexes, and correspond to setting \code{r = 1}, \code{r = 0}, and \code{r = -1} in \code{mean_generalized()}. 

Both \code{x} and \code{w} should be strictly positive (and finite), especially for the purpose of making a price index. This is not enforced, but the results may not make sense if the generalized mean in not defined. There are two exceptions to this.
\enumerate{
\item{The convention in Hardy et al. (1952, p. 13) is used in cases where \code{x} has zeros: the generalized mean is 0 whenever \code{w} is strictly positive and \code{r} < 0. (The analogous convention holds whenever at least one element of \code{x} is \code{Inf}: the generalized mean is \code{Inf} whenever \code{w} is strictly positive and \code{r > 0}.)} 

\item{Some authors let \code{w} be non-negative and sum to 1 (e.g., Sydsaeter et al., 2005, p. 47). If \code{w} has zeros, then the corresponding element of \code{x} has no impact on the mean whenever \code{x} is strictly positive. Unlike \code{\link[stats:weighted.mean]{weighted.mean()}}, however, zeros in \code{w} are not strong zeros, so infinite values in \code{x} will propagate even if the corresponding elements of \code{w} are zero.} 
}

The weights should almost always be scaled to sum to 1 to satisfy the definition of a generalized mean, although there are certain types of price indexes where the weight should not be scaled (e.g., the Vartia-I index).

The underlying calculation returned by \code{mean_generalized()} is mostly identical to \code{\link[stats:weighted.mean]{weighted.mean()}}, with one important exception: missing values in the weights are not treated differently than missing values in \code{x}. Setting \code{na.rm = TRUE} drops missing values in both \code{x} and \code{w}, not just \code{x}. This ensures that certain useful identities are satisfied with missing values in \code{x}. In most cases \code{mean_arithmetic()} is a drop-in replacement for \code{\link[stats:weighted.mean]{weighted.mean()}}.
}

\section{Warning}{
Passing very small values for \code{r} can give misleading results, and warning is given whenever \code{abs(r)} is sufficiently small. In general, \code{r} should not be a computed value.
}

\note{
\code{mean_generalized()} can be defined on the extended real line, so that \code{r = -Inf/Inf} returns \code{\link[base:min]{min()}}/\code{\link[base:max]{max()}}, to agree with the definition in, e.g., Bullen (2003). This is not implemented, and \code{r} must be finite.

There are a number of existing functions for calculating \emph{unweighted} geometric and harmonic means, namely the \code{geometric.mean()} and \code{harmonic.mean()} functions in the \code{'psych'} package, the \code{geomean()} function in the \code{'FSA'} package, the \code{GMean()} and \code{HMean()} functions in the \code{'DescTools'} package, and the \code{geoMean()} function in the \code{'EnvStats'} package. Similarly, the \code{ci_generalized_mean()} function in the \code{'Compind'} package calculates an \emph{unweighted} generalized mean.
}

\references{
Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer Science+Business Media.

Fisher, I. (1922). \emph{The Making of Index Numbers}. Houghton Mifflin Company.

Hardy, G., Littlewood, J. E., and Polya, G. (1952). \emph{Inequalities} (2nd edition). Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.

Lord, N. (2002). Does Smaller Spread Always Mean Larger Product? \emph{The Mathematical Gazette}, 86(506): 273-274.

Sydsaeter, K., Strom, A., and Berck, P. (2005). \emph{Economists' Mathematical Manual} (4th edition). Springer.
}

\seealso{
\code{\link[gpindex]{logmean_generalized}} for the generalized logarithmic mean.

\code{\link[gpindex]{mean_lehmer}} for the Lehmer mean, an alternative to the generalized mean.

\code{\link[gpindex]{weights_transmute}} transforms the weights to turn an r-generalized mean into an s-generalized mean. 

\code{\link[gpindex]{weights_factor}} calculates the weights to factor a mean of products into a product of means.

\code{\link[gpindex]{price_index}} and \code{\link[gpindex]{quantity_index}} for simple wrappers that use \code{mean_generalized()} to calculate common indexes.
}

\examples{
# Make some data

x <- 1:3
w <- c(0.25, 0.25, 0.5)

# Arithmetic mean

mean_arithmetic(x, w) # same as stats::weighted.mean(x, w)

# Geometric mean

mean_geometric(x, w) # same as prod(x^w)

# Using prod() to manually calculate the geometric mean can give misleading
# results

z <- 1:1000
prod(z)^(1 / length(z)) # overflow
mean_geometric(z)

z <- seq(0.0001, by = 0.0005, length.out = 1000)
prod(z)^(1 / length(z)) # underflow
mean_geometric(z)

# Harmonic mean

mean_harmonic(x, w) # same as 1 / stats::weighted.mean(1 / x, w)

# Quadratic mean / root mean square

mean_generalized(2)(x, w)

# Cubic mean
# Notice that this is larger than the other means so far because the 
# generalized mean is increasing in r

mean_generalized(3)(x, w)

#--------------------

# The dispersion between the arithmetic, geometric, and harmonic mean usually
# increases as the variance in 'x' increases

x <- c(1, 3, 5)
y <- c(2, 3, 4)

stats::sd(x) > stats::sd(y)

mean_arithmetic(x) - mean_geometric(x)
mean_arithmetic(y) - mean_geometric(y)

mean_geometric(x) - mean_harmonic(x)
mean_geometric(y) - mean_harmonic(y)

# Example from Lord (2002) where the dispersion decreases as the variance increases,
# counter to the claims in Fisher (1922, p. 108) and the PPI manual (p. 28)

x <- (5 + c(sqrt(5), -sqrt(5), -3)) / 4
y <- (16 + c(7 * sqrt(2), -7 * sqrt(2), 0)) / 16

stats::sd(x) > stats::sd(y)

mean_arithmetic(x) - mean_geometric(x)
mean_arithmetic(y) - mean_geometric(y)

mean_geometric(x) - mean_harmonic(x)
mean_geometric(y) - mean_harmonic(y)

# The "bias" in the arithmetic and harmonic indexes is also smaller in this case, 
# counter to the claim in Fisher (1922, p. 108)

mean_arithmetic(x) * mean_arithmetic(1 / x) - 1
mean_arithmetic(y) * mean_arithmetic(1 / y) - 1

mean_harmonic(x) * mean_harmonic(1 / x) - 1
mean_harmonic(y) * mean_harmonic(1 / y) - 1

#--------------------

# Example of how missing values are handled

w <- replace(w, 2, NA)

mean_arithmetic(x, w)
mean_arithmetic(x, w, na.rm = TRUE) # drops the second observation
stats::weighted.mean(x, w, na.rm = TRUE) # still returns NA

#--------------------

# Sometimes it makes sense to calculate a generalized mean with
# negative inputs, so the warning can be ignored

mean_arithmetic(c(1, 2, -3))

# Other times it's less obvious

mean_harmonic(c(1, 2, -3))
}