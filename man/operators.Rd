\name{operators}
\alias{quantity_index}
\alias{grouped}

\title{Function operators}

\description{
Operators to augment the way a function behaves.
\itemize{
\item{\code{quantity_index()} remaps price arguments into quantity argument (and vice versa) to turn a price index into a quantity index.}
\item{\code{grouped()} makes a function applicable to grouped data.}
}
}

\usage{
quantity_index(f)

grouped(f, ...)
}

\arguments{
\item{f}{A function.}

\item{...}{Additional arguments to \code{f} that should \emph{not} be treated as grouped.}
}

\value{
\code{quantity_index()} returns a function like \code{f}, except that the role of prices/quantities is reversed.

\code{grouped()} returns a function like \code{f} with a new argument \code{group}. This accepts a factor to split all other arguments in \code{f} (except those specified in \code{...}) into groups before applying \code{f} to each group and combining the results. It is similar to \code{\link[=ave]{ave()}}, but more general.
}

\seealso{
\code{\link{price_index}} for the possible functions that can serve as inputs for \code{quantity_index()}.
}

\examples{
p1 <- price6[[3]]
p0 <- price6[[2]]
q1 <- quantity6[[3]]
q0 <- quantity6[[2]]

# Remap argument names to be quantities rather than prices

quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)

laspeyres_index(p1 = q1, p0 = q0, q0 = p0)

# Works with the index_weights() functions, too

quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)

# Calculate Tornqvist weights for two groups

f <- rep(letters[1:2], each = 3)
tornqvist_weights <- grouped(index_weights("Tornqvist"))
tornqvist_weights(p1, p0, q1, q0, group = f)

# Calculate a mean like ave(), but with weights
x <- 1:6
w <- c(1:5, NA)
grouped_mean <- grouped(geometric_mean, na.rm = TRUE)
grouped_mean(x, w, group = f)
}