\name{scale weights}
\alias{weights_scale}
\title{Scale weights}

\description{
Scale a vector of weights so they sum to 1 by calling \code{w / sum(w, na.rm = TRUE)}.
}

\usage{
weights_scale(w)
}

\arguments{
\item{w}{A numeric vector.}
}

\value{
A numeric vector, the same length as \code{w}.
}

\seealso{
\code{\link[gpindex]{weights_transmute}}, \code{\link[gpindex]{weights_factor}}, and \code{\link[gpindex]{index_weights}} for situations when this function may be useful. 
}

\examples{
w <- 1:4
weights_scale(w)
}
