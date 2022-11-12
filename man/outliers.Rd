\name{outliers}
\alias{outliers}
\alias{fixed_cutoff}
\alias{robust_z}
\alias{quartile_method}
\alias{resistant_fences}
\alias{tukey_algorithm}
\alias{hb_transform}

\title{
Outlier methods for price relatives
}

\description{
Standard cutoff-based methods for detecting outliers with price relatives.
}

\usage{
fixed_cutoff(x, cu = 2.5, cl = 1 / cu)

robust_z(x, cu = 2.5, cl = cu)

quartile_method(x, cu = 2.5, cl = cu, a = 0, type = 7)

resistant_fences(x, cu = 2.5, cl = cu, a = 0, type = 7)

tukey_algorithm(x, cu = 2.5, cl = cu, type = 7)

hb_transform(x)
}

\arguments{
\item{x}{A strictly positive numeric vector of price relatives. These can be made with, e.g., \code{\link[=back_period]{back_period()}}.}
\item{cu, cl}{A numeric vector giving the upper and lower cutoffs for each element of \code{x}. The usual recycling rules apply.}
\item{a}{A numeric vector between 0 and 1 giving the scale factor for the median to establish the minimum dispersion between quartiles for each element of \code{x}. The default does not set a minimum dispersion. The usual recycling rules apply.}
\item{type}{See \code{\link{quantile}}.}
}

\details{
Each of these functions constructs an interval of the form \eqn{[b_l(x) - c_l \times l(x), b_u(x) + c_u \times u(x)]}{[bl(x) - cl * l(x), bu(x) + cu * u(x)]} and assigns a value in \code{x} as \code{TRUE} if that value does not belong to the interval, \code{FALSE} otherwise. The methods differ in how they construct the values \eqn{b_l(x)}{bl(x)}, \eqn{b_u(x)}{bu(x)}, \eqn{l(x)}, and \eqn{u(x)}. Any missing values in \code{x} are ignored when calculating the cutoffs, but will return \code{NA}.

The fixed cutoff method is the simplest, and just uses the interval \eqn{[c_l, c_u]}{[cl, cu]}.

The quartile method and Tukey algorithm are described in paragraphs 5.113 to 5.135 of the CPI manual (2020), as well as by Rais (2008) and Hutton (2008). The resistant fences method is an alternative to the quartile method, and is described by Rais (2008) and Hutton (2008). Quantile-based methods often identify price relatives as outliers because the distribution is concentrated around 1; setting \code{a > 0} puts a floor on the minimum dispersion between quantiles as a fraction of the median. See the references for more details.

The robust Z-score is the usual method to identify relatives in the (asymmetric) tails of the distribution, simply replacing the mean with the median, and the standard deviation with the median absolute deviation.

These methods often assume that price relatives are symmetrically distributed (if not Gaussian). As the distribution of price relatives often has a long right tail, the natural logarithm can be used to transform price relative before identifying outliers (sometimes under the assumption that price relatives are distributed log-normal). The Hidiroglou-Berthelot transformation is another approach, described in the CPI manual (par. 5.124).
}

\value{
A logical vector, the same length as \code{x}, that is \code{TRUE} if the corresponding element of \code{x} is identified as an outlier, \code{FALSE} otherwise.
}

\references{
Hutton, H. (2008). Dynamic outlier detection in price index surveys. \emph{Proceedings of the Survey Methods Section: Statistical Society of Canada Annual Meeting}.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

Rais, S. (2008). Outlier detection for the Consumer Price Index. \emph{Proceedings of the Survey Methods Section: Statistical Society of Canada Annual Meeting}. 
}

\seealso{
\code{\link{grouped}} to make each of these functions operate on grouped data.

\code{\link{back_period}}/\code{\link{base_period}} for a simple utility function to turn prices in a table into price relatives.
}

\examples{
set.seed(1234)

x <- rlnorm(10)

fixed_cutoff(x)
robust_z(x)
quartile_method(x)
resistant_fences(x) # always identifies fewer outliers than above
tukey_algorithm(x)

log(x)
hb_transform(x)

# Works the same for grouped data

f <- c("a", "b", "a", "a", "b", "b", "b", "a", "a", "b")
grouped(quartile_method)(x, group = f)
}
