% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contributions.R
\name{contributions}
\alias{contributions}
\alias{arithmetic_contributions}
\alias{geometric_contributions}
\alias{harmonic_contributions}
\alias{nested_contributions}
\alias{nested_contributions2}
\alias{fisher_contributions}
\alias{fisher_contributions2}
\title{Percent-change contributions}
\usage{
contributions(r)

arithmetic_contributions(x, w = NULL)

geometric_contributions(x, w = NULL)

harmonic_contributions(x, w = NULL)

nested_contributions(r1, r2, t = c(1, 1))

nested_contributions2(r1, r2, t = c(1, 1))

fisher_contributions(x, w1 = NULL, w2 = NULL)

fisher_contributions2(x, w1 = NULL, w2 = NULL)
}
\arguments{
\item{r}{A finite number giving the order of the generalized mean.}

\item{x}{A strictly positive numeric vector.}

\item{w, w1, w2}{A strictly positive numeric vector of weights, the same length
as \code{x}. The default is to equally weight each element of \code{x}.}

\item{r1}{A finite number giving the order of the outer generalized mean.}

\item{r2}{A pair of finite numbers giving the order of the inner generalized
means.}

\item{t}{A pair of strictly positive weights for the inner generalized
means. The default is equal weights.}
}
\value{
\code{contributions()} returns a function:

\preformatted{function(x, w = NULL){...}}

\code{nested_contributions()} and \code{nested_contributions2()} return a
function:

\preformatted{function(x, w1 = NULL, w2 = NULL){...}}

\code{arithmetic_contributions()}, \code{geometric_contributions()},
\code{harmonic_contributions()}, \code{fisher_contributions()}, and
\code{fisher_contributions2()} each return a numeric vector, the same
length as \code{x}.
}
\description{
Calculate additive percent-change contributions for generalized-mean price
indexes, and indexes that nest two levels of generalized means consisting of
an outer generalized mean and two inner generalized means (e.g., the Fisher
index).
}
\details{
The function \code{contributions()} is a simple wrapper for
\code{\link[=transmute_weights]{transmute_weights(r, 1)()}} to calculate
(additive) percent-change contributions for a price index based on a
generalized mean of order \code{r}. It returns a function to compute a
vector \code{v(x, w)} such that

\preformatted{generalized_mean(r)(x, w) - 1 == sum(v(x, w))}

The \code{arithmetic_contributions()}, \code{geometric_contributions()} and
\code{harmonic_contributions()} functions cover the most important cases
(i.e., \code{r = 1}, \code{r = 0}, and \code{r = -1}).

The \code{nested_contributions()} and \code{nested_contributions2()}
functions are the analog of \code{contributions()} for an index based on a
nested generalized mean with two levels, like a Fisher index. They are
wrappers around \code{nested_transmute()} and \code{nested_transmute2()}, respectively.

The \code{fisher_contributions()} and \code{fisher_contributions2()}
functions correspond to \code{nested_contributions(0, c(1, -1))()} and
\code{nested_contributions2(0, c(1, -1))()}, and are appropriate for
calculating percent-change contributions for a Fisher index.
}
\examples{
p2 <- price6[[2]]
p1 <- price6[[1]]
q2 <- quantity6[[2]]
q1 <- quantity6[[1]]

# Percent-change contributions for the Jevons index.

geometric_mean(p2 / p1) - 1

geometric_contributions(p2 / p1)

all.equal(
  geometric_mean(p2 / p1) - 1,
  sum(geometric_contributions(p2 / p1))
)

# Percent-change contributions for the Fisher index in section 6 of
# Reinsdorf et al. (2002).

(con <- fisher_contributions(p2 / p1, p1 * q1, p2 * q2))

all.equal(sum(con), fisher_index(p2, p1, q2, q1) - 1)

# Not the only way.

(con2 <- fisher_contributions2(p2 / p1, p1 * q1, p2 * q2))

all.equal(sum(con2), fisher_index(p2, p1, q2, q1) - 1)

# The same as the van IJzeren decomposition in section 4.2.2 of
# Balk (2008).

Qf <- quantity_index(fisher_index)(q2, q1, p2, p1)
Ql <- quantity_index(laspeyres_index)(q2, q1, p1)
wl <- scale_weights(p1 * q1)
wp <- scale_weights(p1 * q2)

(Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * (p2 / p1 - 1)

# Similar to the method in section 2 of Reinsdorf et al. (2002),
# although those contributions aren't based on weights that sum to 1.

Pf <- fisher_index(p2, p1, q2, q1)
Pl <- laspeyres_index(p2, p1, q1)

(1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p2 / p1 - 1)

# Also similar to the decomposition by Hallerbach (2005), noting that
# the Euler weights are close to unity.

Pp <- paasche_index(p2, p1, q2)

(0.5 * sqrt(Pp / Pl) * wl + 0.5 * sqrt(Pl / Pp) * wp) * (p2 / p1 - 1)
}
\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}.
Cambridge University Press.

Hallerbach, W. G. (2005). An alternative decomposition of the Fisher index.
\emph{Economics Letters}, 86(2):147--152

Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive
decompositions for Fisher, Törnqvist and geometric mean indexes.
\emph{Journal of Economic and Social Measurement}, 28(1-2):51--61.
}
\seealso{
\code{\link[=transmute_weights]{transmute_weights()}} for the underlying implementation.
}
