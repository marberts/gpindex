\docType{package}
\name{gpindex-package}
\alias{gpindex-package}
\alias{gpindex}

\title{Generalized+ Price and Quantity Indices}

\description{
A small package for calculating lots of different price and quantity indices. Provides tools to build and work with any type of generalized bilateral index (of which most price and quantity indices are), along with a few important indices that don't belong to the generalized family. 
}

\details{
To avoid duplication, everything is framed as a price index; it is trivial to turn a price index into its analogous quantity index by simply switching prices and quantities.

All generalized indices are based on the generalized mean, which is provided by the \code{\link[gpindex]{mean_generalized}} function. Given a set of price relatives and weights, any generalized price index is easily calculated as a generalized mean. Two important functions for decomposing generalized means are given by \code{\link[gpindex]{weights_change}} and \code{\link[gpindex]{weights_factor}}. These functions can be used to calculate quote contributions and price-update weights for generalized indices. Together, these three functions, along with \code{\link[gpindex]{logmean_generalized}}, provide the key mathematical apparatus to work with any type of generalized index. 

On top of these basic mathematical tools are functions that are relevant for making standard price indices when both prices and quantities are known. Weights for a large variety of indices can be calculated with \code{\link[gpindex]{index_weights}}, which can be plugged into the relevant generalized mean to calculate most common price indices, and many uncommon ones. The \code{\link[gpindex]{index}} functions provide a simple wrapper.

There are a number of R packages on the CRAN for working with price/quantity indices (e.g., 'IndexNumber', 'productivity', 'IndexNumR', 'micEconIndex'). Compared to existing libraries, this package provides greater flexibility for building index numbers in the class of generalized price and quantity indices. While it supports a large number of index-number formulas out-of-the box, the focus is on the tools to easily make and work with any type of generalized price index. No assumptions are made about how data are stored or arranged; rather, the functions in the package are designed to work with atomic vectors, and can be used with R's standard data-manipulation functions for more complex data structures. Compared to existing libraries, this library is suitable for building custom price/quantity indices, and learning about different types of index-number formulas.
}

\seealso{
\url{https://github.com/marberts/gpindex}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}
}
