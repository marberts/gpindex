\name{scale weights}
\alias{weights_scale}
\title{Scale weights}

\description{
Scale a vector weights so they sum to 1.
}

\usage{
weights_scale(w, na.rm = FALSE)
}

\arguments{
\item{w}{A numeric vector.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{w}.
}

\details{
This function is a simple way to call \code{sum(w) / sum(w, na.rm = na.rm)}.
}

\seealso{
\code{\link[gpindex]{weights_change}} and \code{\link[gpindex]{weights_factor}} for an application. 
}

\examples{
w <- 1:4
weights_scale(w)
}
