

#' Generalized Price and Quantity Indexes
#' 
#' A small package for calculating lots of different price indexes, and by
#' extension quantity indexes. Provides tools to build and work with any type
#' of bilateral generalized-mean index (of which most price indexes are), along
#' with a few important indexes that don't belong to the generalized-mean
#' family (e.g., superlative quadratic-mean indexes, GEKS). Implements and
#' extends many of the methods in Balk (2008), von der Lippe (2001), and the
#' CPI manual (2020) for bilateral price indexes.
#' 
#' 
#' @name gpindex-package
#' @aliases gpindex-package gpindex
#' @docType package
#' @section Background: Everything is framed as a price index to avoid
#' duplication; it is trivial to turn a price index into its analogous quantity
#' index by simply switching prices and quantities.
#' 
#' Generalized-mean price indexes (sometimes called generalized price indexes
#' for short) are a large family of price indexes with nice properties, such as
#' the mean-value and identity properties (e.g., Balk, 2008, Chapter 3). When
#' used with value-share weights, these indexes satisfy the key homogeneity
#' properties, commensurability, and are consistent in aggregation. This last
#' feature makes generalized-mean indexes natural candidates for making
#' national statistics, and this justifies the hierarchical structure used by
#' national statistical agencies for calculating and disseminating collections
#' of price indexes.
#' 
#' Almost all bilateral price indexes used in practice are either
#' generalized-mean indexes (like the Laspeyres and Paasche index) or are
#' nested generalized-mean indexes (like the Fisher index).
#' @author **Maintainer**: Steve Martin \email{stevemartin041@@gmail.com}
#' @seealso <https://github.com/marberts/gpindex>
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
#' Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical Office,
#' Wiesbaden.
#' 
#' Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
#' Stochastic Approach*. MacMillan.
