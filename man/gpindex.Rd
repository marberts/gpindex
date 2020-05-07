\docType{package}
\name{gpindex}
\alias{gpindex}

\title{Generalized+ price and quantity indices}

\description{
A small package for calculating lots of different price and quantity indices. To avoid duplication, everything is framed as a price index; it is trivial to turn a price index into its analogous quantity index by simply switching prices and quantites.

There are a number of R packages on the CRAN for working with price/quantity indices (e.g., IndexNumber, productivity, IndexNumR, micEconIndex). Compared to existing libraries, this package provides greater flexibility for building a custom index-estimation system. While it support a large number of index-number formulas out-of-the box, the focus is on the tools to easily make any type of generalized price index (of which most price indices are). No assumptions are made about how data are stored or arranged; rather, the various functions in the package are designed to be used with R's standard data-manipulation functions. Finally, this packages provides support for some important decompositions that are needed to correctly calculate product contributions.

}

\seealso{
\url{https://github.com/marberts/gpindex}
}

\author{
\strong{Maintainer}: Steve Martin \email{stevemartin041@gmail.com}
}
