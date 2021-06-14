\name{quantity indexes}
\alias{quantity_index}

\title{Quantity indexes}

\description{
Remap price arguments into quantity argument (and vice versa) to turn a price index into a quantity index.
}

\usage{
quantity_index(price_index)
}

\arguments{
\item{price_index}{A function to compute a price index. See \code{\link{price_index}}.}
}

\value{
A function like \code{price_index}, except that the role of prices/quantities is reversed.
}

\seealso{
\code{\link{price_index}} for the possible functions that can serve as inputs.
}

\examples{
p0 <- price6[[2]]
q1 <- quantity6[[3]]
q0 <- quantity6[[2]]

# Just remaps argument names to be quantities rather than prices

quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)

laspeyres_index(p1 = q1, p0 = q0, q0 = p0)

# Works with the index_weights() functions, too

quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
}