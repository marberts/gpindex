\name{operators}
\alias{quantity_index}
\alias{grouped}
\alias{balanced}

\title{Function operators}

\description{
Operators to augment the way a function behaves.
\itemize{
\item{\code{quantity_index()} remaps price arguments into quantity argument (and vice versa) to turn a price index into a quantity index.}
\item{\code{grouped()} makes a function applicable to grouped data.}
\item{\code{balanced()} makes a function balance the removal of \code{NA}s across multiple input vectors.}
}
}

\usage{
quantity_index(f)

grouped(f, ...)

balanced(f, ...)
}

\arguments{
\item{f}{A function.}

\item{...}{Additional arguments to \code{f} that should \emph{not} be treated as grouped / should \emph{not} be balanced.}
}

\value{
\code{quantity_index()} returns a function like \code{f}, except that the role of prices/quantities is reversed.

\code{grouped()} returns a function like \code{f} with a new argument \code{group}. This accepts a factor to split all other arguments in \code{f} (except those specified in \code{...}) into groups before applying \code{f} to each group and combining the results. It is similar to \code{\link[=ave]{ave()}}, but more general.

\code{balanced()} returns a function like \code{f}, except that missing values are balanced across \emph{all} inputs when \code{na.rm = TRUE} (except those specified in \code{...}). This is like using \code{\link[=complete.cases]{complete.cases()}} on the inputs of \code{f}.
}

\note{
Additional arguments passed to \code{...} are evaluated, so the return function doesn't evaluate these arguments lazily. In most cases these additional arguments are parameters like \code{elasticity} or switches like \code{na.rm}, and eager evaluation prevents these from being changed in, e.g., a loop. Lazy evaluation can be had by passing an anonymous function that partials out these arguments.
}

\seealso{
\code{\link{price_index}} for the possible functions that can serve as inputs for \code{quantity_index()} and \code{balanced()}.
}

\examples{
p1 <- price6[[3]]
p0 <- price6[[2]]
q1 <- quantity6[[3]]
q0 <- quantity6[[2]]

#---- Quantity index ----
# Remap argument names to be quantities rather than prices

quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)

laspeyres_index(p1 = q1, p0 = q0, q0 = p0)

# Works with the index_weights() functions, too

quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)

#---- Grouped ----
# Calculate Tornqvist weights for two groups

f <- factor(rep(letters[1:2], each = 3))
tornqvist_weights <- grouped(index_weights("Tornqvist"))
tornqvist_weights(p1, p0, q1, q0, group = f)

# Calculate a mean like ave(), but with weights

x <- 1:6
w <- c(1:5, NA)
grouped_mean <- grouped(geometric_mean, na.rm = TRUE)
grouped_mean(x, w, group = f)

# Redistribute weights

w1 <- c(2, 4)
w2 <- 1:6

harmonic_mean(mapply(harmonic_mean, split(x, f), split(w2, f)), w1)

wr <- grouped(scale_weights)(w2, group = f) * w1[f]
harmonic_mean(x, wr)

#---- Balanced ----
# Balance missing values for a Fisher index

fisher <- balanced(fisher_index)
fisher(p1, p0, q1, replace(q0, 3, NA), na.rm = TRUE)
fisher_index(p1[-3], p0[-3], q1[-3], q0[-3])

# Operators can be combined, but some care may be needed

grouped(balanced(fisher_mean), na.rm = TRUE)(x, w, group = f)
balanced(grouped(fisher_mean))(x, w, group = f, na.rm = TRUE)
}