\name{quantity indexes}
\alias{quantity_index}

\title{Quantity indexes}

\description{
It is trivial to turn a price index into a quantity index by switching the role of prices and quantities in the index-number formula. This function does just that.
}

\usage{
quantity_index(price_index)
}

\arguments{
\item{price_index}{A function to compute a price index. See \code{\link[gpindex]{price_index}}.}
}

\value{
A function like \code{price_index}, except that the role of prices/quantities has been reversed.
}

\seealso{
\code{\link[gpindex]{price_index}} for the possible functions that can serve as inputs.
}

\examples{
# Make some data

p0 <- price6[[2]]
q1 <- quantity6[[3]]
q0 <- quantity6[[2]]

# Just remaps argument names to be quantities rather than prices

quantity_index(index_laspeyres)(q1 = q1, q0 = q0, p0 = p0)

index_laspeyres(p1 = q1, p0 = q0, q0 = p0)

# Works with the index_weights() functions, too

quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
}