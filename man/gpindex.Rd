\docType{package}
\name{gpindex-package}
\alias{gpindex-package}
\alias{gpindex}

\title{Generalized Price and Quantity Indexes}

\description{
A small package for calculating lots of different price indexes, and by extension quantity indexes. Provides tools to build and work with any type of generalized bilateral index (of which most price indexes are), along with a few important indexes that don't belong to the generalized family. Implements and extends many of the methods in Balk (2008, ISBN:978-1-107-40496-0) and ILO, IMF, OECD, Eurostat, UN, and World Bank (2004, ISBN:92-2-113699-X) for bilateral price indexes.
}

\section{Usage}{
To avoid duplication, everything is framed as a price index; it is trivial to turn a price index into its analogous quantity index by simply switching prices and quantities.

Generalized indexes (also called generalized-mean indexes) are a large family of price indexes with nice properties (e.g., Balk, 2008, Chapter 3). Almost all bilateral price indexes used in practice are either generalized indexes (like the Laspeyres and Paasche index) or are nested generalized indexes (like the Fisher index).

All generalized indexes are based on the generalized mean, which is provided by the \code{\link[=generalized_mean]{generalized_mean()}} function. Given a set of price relatives and weights, any generalized price index is easily calculated as a generalized mean.

Two important functions for decomposing generalized means are given by \code{\link[=transmute_weights]{transmute_weights()}} and \code{\link[=factor_weights]{factor_weights()}}. These functions can be used to calculate quote contributions (with, e.g., \code{\link[=contributions]{contributions()}}) and price-update weights for generalized indexes. 

Together these functions, along with \code{\link[=generalized_logmean]{generalized_logmean()}}, provide the key mathematical apparatus to work with any type of generalized index, and those that are nested generalized indexes. See the vignette for more details: \code{vignette("gpindex")}.

On top of these basic mathematical tools are functions for making standard price indexes when both prices and quantities are known. Weights for a large variety of indexes can be calculated with \code{\link[=index_weights]{index_weights()}}, which can be plugged into the relevant generalized mean to calculate most common price indexes, and many uncommon ones. The \code{\link{price_index}} functions provide a simple wrapper, with the \code{\link[=quantity_index]{quantity_index()}} function turning each of these into its analogous quantity index.
}

\note{
There are a number of R packages on the CRAN for working with price/quantity indexes (e.g., \pkg{IndexNumber}, \pkg{productivity}, \pkg{IndexNumR}, \pkg{micEconIndex}). Compared to existing packages, this package provides greater flexibility for building index numbers in the class of generalized price and quantity indexes. 

While there is support for a large number of index-number formulas out-of-the box, the focus is on the tools to easily make and work with any type of generalized price index. Consequently, compared to existing packages, this package is suitable for building custom price/quantity indexes, and learning about or researching different types of index-number formulas.
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