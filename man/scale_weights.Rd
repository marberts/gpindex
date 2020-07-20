\name{scale weights}
\alias{weights_scale}
\title{Scale weights}

\description{
Scale a vector of weights so they sum to 1.
}

\usage{
weights_scale(w, na.rm = FALSE)
}

\arguments{
\item{w}{A numeric or logical vector.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{w}.
}

\details{
This function is a simple way to call \code{w / sum(w)}.

To speed up execution when there are NAs in \code{w}, the return value is always a vector of NAs whenever \code{na.rm = FALSE} and \code{anyNA(w) == TRUE}. This means that NaNs can be handled slightly differently than \code{w / sum(w)}.
}

\seealso{
\code{\link[gpindex]{weights_change}}, \code{\link[gpindex]{weights_factor}}, and \code{\link[gpindex]{index_weights}} for an application. 
}

\examples{
w <- 1:4
weights_scale(w)
}
