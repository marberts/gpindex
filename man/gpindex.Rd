\docType{package}
\name{gpindex-package}
\alias{gpindex-package}
\alias{gpindex}

\title{Generalized Price and Quantity Indexes}

\description{
A small package for calculating lots of different price indexes, and by extension quantity indexes. Provides tools to build and work with any type of bilateral generalized-mean index (of which most price indexes are), along with a few important indexes that don't belong to the generalized-mean family. Implements and extends many of the methods in Balk (2008, ISBN:978-1-107-40496-0) and ILO, IMF, OECD, Eurostat, UN, and World Bank (2020, ISBN:978-1-51354-298-0) for bilateral price indexes.
}

\section{Background}{
Everything is framed as a price index to avoid duplication; it is trivial to turn a price index into its analogous quantity index by simply switching prices and quantities.

Generalized-mean price indexes (sometimes called generalized price indexes for short) are a large family of price indexes with nice properties, such as the mean-value and identity properties (e.g., Balk, 2008, Chapter 3). Generalized-mean indexes are natural candidates for making national statistics, as these indexes are consistent in aggregation, and this justifies the hierarchical structure used by national statistical agencies for calculating and disseminating collections of price indexes. Almost all bilateral price indexes used in practice are either generalized-mean indexes (like the Laspeyres and Paasche index) or are nested generalized-mean indexes (like the Fisher index).
}

\section{Usage}{
A generalized-mean price index is a weighted generalized mean of price relatives. Given a set of price relatives and weights, any generalized-mean price index is easily calculated with the \code{\link[=generalized_mean]{generalized_mean()}} function. What distinguishes different generalized-mean price indexes are the weights and the order of the generalized mean. For example, the standard Laspeyres index uses base-period expenditure/revenue share weights in a generalized mean of order 1 (arithmetic mean). Changing the order of the generalized mean to \eqn{1 - \sigma}{1 - \sigma}, where \eqn{\sigma}{\sigma} is an elasticity of substitution, gives a Lloyd-Moulton index, whereas changing the weights to current-period expenditure/revenue shares gives a Palgrave index. 

Generalized-mean indexes can also be nested together to get indexes like the Fisher or AG mean index. The \code{\link[=nested_mean]{nested_mean()}} function is a simple wrapper for \code{\link[=generalized_mean]{generalized_mean()}} for these cases.

Two important functions for decomposing generalized means are given by \code{\link[=transmute_weights]{transmute_weights()}} and \code{\link[=factor_weights]{factor_weights()}}. These functions augment the weights in a generalized mean, and can be used to calculate quote contributions (with, e.g., \code{\link[=contributions]{contributions()}}) and price-update weights for generalized-mean indexes. Together these functions provide the key mathematical apparatus to work with any generalized-mean index, and those that are nested generalized-mean indexes.

On top of these basic mathematical tools are functions for making standard price indexes when both prices and quantities are known. Weights for a large variety of indexes can be calculated with \code{\link[=index_weights]{index_weights()}}, which can be plugged into the relevant generalized mean to calculate most common price indexes, and many uncommon ones. The \code{\link{price_index}} functions provide a simple wrapper, with the \code{\link[=quantity_index]{quantity_index()}} function turning each of these into its analogous quantity index.

There are several utility functions for making price relatives (e.g., \code{\link[=back_price]{back_price()}}) when data are stored in a table, and identifying extreme price relatives (e.g., \code{\link[=resistant_fences]{resistant_fences()}}) that may not be suitable for a price index.
}

\note{
There are a number of R packages on the CRAN for working with price/quantity indexes that implement the standard index-number formulas (e.g., \pkg{IndexNumber}, \pkg{productivity}, \pkg{IndexNumR}, \pkg{micEconIndex}). While there is support for a large number of index-number formulas out-of-the box in this package, the focus is on the tools to easily make and work with any type of generalized-mean price index. Consequently, compared to existing packages, this package is suitable for building custom price/quantity indexes, and learning about or researching different types of index-number formulas.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\url{https://github.com/marberts/gpindex}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}
}