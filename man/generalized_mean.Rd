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
mean_generalized(x, w = rep(1, length(x)), r, na.rm = FALSE, scale = TRUE)

mean_arithmetic(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

mean_geometric(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)

mean_harmonic(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{x}{A numeric or logical vector.}

\item{w}{A vector of numeric or logical weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{r}{A number giving the exponent of the generalized mean.}

\item{na.rm}{Should missing values in \code{x} and \code{w} be removed?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
A numeric value.
}

\details{
The function \code{mean_generalized()} returns the value of the generalized mean of \code{x} with weights \code{w} and exponent \code{r} (i.e., the weighted mean of \code{x} to the power of \code{r}, all raised to the power of 1 / \code{r}). This is also called the power mean or Holder mean. See Bullen (2003, p. 175) for a precise definition, or \url{https://en.wikipedia.org/wiki/Power_mean}. 

The functions \code{mean_arithmetic()}, \code{mean_geometric()}, and \code{mean_harmonic()} are the most useful generalized means, and correspond to setting \code{r = 1}, \code{r = 0}, and \code{r = -1} in \code{mean_generalized()}. 

As a matter of definition, both \code{x} and \code{w} should be strictly positive, especially for the purpose of making a price index. This is not enforced here, but the results may not make sense if the generalized mean in not defined. There are two exceptions to this.
\enumerate{
\item{The convention in Hardy et al. (1934, p. 13) is used in cases where \code{x} has zeros: the generalized mean is 0 whenever \code{w} is strictly positive and \code{r} < 0.} 

\item{Some authors let \code{w} be non-negative and sum to 1 (e.g., Sydsaeter et al., 2005, p. 47). If \code{w} has zeros, then the corresponding element of \code{x} has no impact on the mean, unless it is missing (unlike \code{\link[stats:weighted.mean]{weighted.mean()}}).} 
}

The weights should almost always be scaled to sum to 1 to satisfy the definition of a generalized mean, although there are certain types of price indexes where the weight should not be scaled (e.g., the Vartia-I index).

The underlying calculation for \code{mean_generalized()} is mostly identical to \code{\link[stats:weighted.mean]{weighted.mean()}}, with a few exceptions. 
\enumerate{
\item{Missing values in the weights are not treated differently than missing values in \code{x}. Setting \code{na.rm = TRUE} drops missing values in both \code{x} and \code{w}, not just \code{x}. This ensures that certain useful identities are satisfied with missing values in \code{x}.}
\item{To speed up execution when there are NAs in \code{x} or \code{w}, the return value is always NA whenever \code{na.rm = FALSE} and \code{anyNA(x) == TRUE} or \code{anyNA(w) == TRUE}. This means that NaNs can be handled slightly differently than \code{\link[stats:weighted.mean]{weighted.mean()}}.}
}
In most cases \code{mean_arithmetic()} is a drop-in replacement for \code{\link[stats:weighted.mean]{weighted.mean()}}.
}

\note{
There are a number of existing functions for calculating \emph{unweighted} geometric and harmonic means, namely the \code{geometric.mean()} and \code{harmonic.mean()} functions in the \code{'psych'} package, the \code{geomean()} function in the \code{'FSA'} package, the \code{GMean()} and \code{HMean()} functions in the \code{'DescTools'} package, and the \code{geoMean()} function in the \code{'EnvStats'} package. 
}

\references{
Bullen, P. S. (2003). \emph{Handbook of Means and Their Inequalities}. Springer.

Hardy, G., Littlewood, J. E., and Polya, G. (1934). \emph{Inequalities}. Cambridge University Press.

Lord, N. (2002). Does Smaller Spread Always Mean Larger Product? \emph{The Mathematical Gazette}, 86(506): 273-274.

Sydsaeter, K., Strom, A., and Berck, P. (2005). \emph{Economists' Mathematical Manual} (4th edition). Springer.
}

\seealso{
\code{\link[gpindex]{logmean_generalized}} for the generalized logarithmic mean. 

\code{\link[gpindex]{weights_change}} can be used to turn an r-generalized mean into a k-generalized mean. 

\code{\link[gpindex]{weights_factor}} can be used to factor the weights to a turn a mean of products into a product of means.
}

\examples{
# Arithmetic mean

x <- 1:3
w <- c(0.25, 0.25, 0.5)

mean_arithmetic(x, w)
stats::weighted.mean(x, w) # same as stats::weighted.mean

# Geometric mean

mean_geometric(x, w)
prod(x^w) # same as manual calculation

# Using prod to manually calculate the geometric mean can give misleading results

z <- 1:1000
prod(z)^(1 / length(z)) # overflow
mean_geometric(z)

z <- seq(0.0001, by = 0.0005, length.out = 1000)
prod(z)^(1 / length(z)) # underflow
mean_geometric(z)

# Harmonic mean

mean_harmonic(x, w)
1 / stats::weighted.mean(1 / x, w) # same as manual calculation

# Quadratic mean / root mean sqaure

mean_generalized(x, w, r = 2)

# Cubic mean

mean_generalized(x, w, r = 3)

# Example of how missing values are handled

mean_arithmetic(x, c(0.25, NA, 0.5))
mean_arithmetic(x, c(0.25, NA, 0.5), na.rm = TRUE) # stats::weighted.mean returns NA

# Example from Lord (2002) where the dispersion between arithmetic and geometric means
# decreases as the variance increases

x <- c((5 + sqrt(5)) / 4, (5 - sqrt(5)) / 4, 1 / 2)
y <- c((16 + 7 * sqrt(2)) / 16, (16 - 7 * sqrt(2)) / 16, 1)

stats::sd(x) > stats::sd(y)
mean_arithmetic(x) - mean_geometric(x) < mean_arithmetic(y) - mean_geometric(y)
}