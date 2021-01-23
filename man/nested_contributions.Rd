\name{nested contributions}
\alias{contributions_nested}

\title{
Nested contributions
}

\description{
Calculate additive quote contributions for a price index that nests two levels of generalized means, like the Fisher index, consisting of an outer generalized mean and an collection of inner generalized means.
}

\usage{
contributions_nested(r1, r2, w1 = rep(1, length(r2)))
}

\arguments{
\item{r1}{A finite number giving the order of the outer generalized mean.}
\item{r2}{A vector of finite numbers giving the order of the inner generalized means.}
\item{w1}{A vector of numeric weights that weights each inner generalized mean in the outer generalized mean, the same length as \code{r2}.}
}

\details{
This function is the analog of \code{\link[=contributions]{contributions()}} for a nested generalized mean with two levels, like a Fisher or Harmonic Laspeyres Paasche index. That is, it calculates the contribution of each element of \code{x} when a generalized mean of order \code{r1}, with weights \code{w1}, aggregates a collection of generalized means of \code{x} with orders in the list \code{r2}, each with weights in the list \code{w}.
}

\value{
\code{contributions_nested()} returns a function:

\code{function(x, w = rep(list(rep(1, length(x))), length(r2)))}.

This function takes a numeric vector \code{x} and list of numeric weights \code{w}, the same length as \code{r2}, and returns the contribution for each element in \code{x}.
}

\note{
This function is experimental, and the interface may change in future versions.
}

\seealso{
\code{\link{contributions}} for contributions without nesting.
}

\examples{
p1 <- price6[[2]]
p0 <- price6[[1]]
q1 <- quantity6[[2]]
q0 <- quantity6[[1]]

contributions_fisher <- contributions_nested(0, c(1, -1))
contributions_fisher(p1 / p0, list(index_weights("Laspeyres")(p0, q0), 
                                   index_weights("Paasche")(p1, q1)))
                                   
contributions_hlp <- contributions_nested(-1, c(1, -1))
contributions_hlp(p1 / p0, list(index_weights("Laspeyres")(p0, q0), 
                                index_weights("Paasche")(p1, q1)))
}