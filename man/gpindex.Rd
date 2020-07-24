\docType{package}
\name{gpindex-package}
\alias{gpindex-package}
\alias{gpindex}

\title{Generalized Price and Quantity Indexes}

\description{
A small package for calculating lots of different price indexes, and by extension quantity indexes. Provides tools to build and work with any type of generalized bilateral index (of which most price indexes are), along with a few important indexes that don't belong to the generalized family. Implements and extends many of the methods in Balk (2008, ISBN:978-1-107-40496-0) and ILO, IMF, OECD, Eurostat, UN, and World Bank (2004, ISBN:92-2-113699-X) for bilateral price indexes.
}

\details{
To avoid duplication, everything is framed as a price index; it is trivial to turn a price index into its analogous quantity index by simply switching prices and quantities.

Generalized indexes are a large family of price indexes that are consistent in aggregation (Balk, 2008, section 3.7.3). Almost all bilateral price indexes used in practice are either generalized indexes (like the Laspeyres and Paasche index) or are nested generalized indexes (like the Fisher index).

All generalized indexes are based on the generalized mean, which is provided by the \code{\link[gpindex:mean_generalized]{mean_generalized()}} function. Given a set of price relatives and weights, any generalized price index is easily calculated as a generalized mean. 

Two important functions for decomposing generalized means are given by \code{\link[gpindex:weights_change]{weights_change()}} and \code{\link[gpindex:weights_factor]{weights_factor()}}. These functions can be used to calculate quote contributions and price-update weights for generalized indexes. 

Together these functions, along with \code{\link[gpindex:logmean_generalized]{logmean_generalized()}}, provide the key mathematical apparatus to work with any type of generalized index, and those that are nested generalized indexes.

On top of these basic mathematical tools are functions for making standard price indexes when both prices and quantities are known. Weights for a large variety of indexes can be calculated with \code{\link[gpindex:index_weights]{index_weights()}}, which can be plugged into the relevant generalized mean to calculate most common price indexes, and many uncommon ones. The \code{\link[gpindex]{index}} functions provide a simple wrapper.
}

\note{
There are a number of R packages on the CRAN for working with price/quantity indexes (e.g., 'IndexNumber', 'productivity', 'IndexNumR', 'micEconIndex'). Compared to existing libraries, this package provides greater flexibility for building index numbers in the class of generalized price and quantity indexes. 

While there is support for a large number of index-number formulas out-of-the box, the focus is on the tools to easily make and work with any type of generalized price index. No assumptions are made about how data are stored or arranged; rather, the functions in the package are designed to work with atomic vectors, and can be used with R's standard data-manipulation functions for more complex data structures. Compared to existing libraries, this library is suitable for building custom price/quantity indexes, and learning about different types of index-number formulas.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\url{https://github.com/marberts/gpindex}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}
}