\docType{data}
\name{price/quantity data}
\alias{price6}
\alias{quantity6}

\title{Sample price/quantity data}

\description{
Prices and quantities for six products over five periods.
}

\usage{
price6
quantity6
}

\format{
Each data frame has 6 rows and 5 columns, with each row corresponding to a product and each column corresponding to a time period.
}

\note{
Adapted from tables 3.1 and 3.2 in Balk (2008), which were adapted from tables 19.1 and 19.2 in the PPI manual.
}

\source{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\examples{
# Recreate tables 3.4, 3.6, and 3.12 from Balk (2008)

index_formulas <- function(p1, p0, q1, q0) {
  c(harmonic_laspeyres = index_harmonic("Laspeyres")(p1, p0, q0),
    geometric_laspeyres = index_geometric("Laspeyres")(p1, p0, q0),
    laspeyres = index_arithmetic("Laspeyres")(p1, p0, q0),
    paasche = index_harmonic("Paasche")(p1, p0, q1),
    geometric_paasche = index_geometric("Paasche")(p1, p0, q1),
    palgrave = index_arithmetic("Palgrave")(p1, p0, q1),
    fisher = index_fisher(p1, p0, q1, q0),
    tornqvist = index_geometric("Tornqvist")(p1, p0, q1, q0),
    marshall_edgeworth = index_arithmetic("MarshallEdgeworth")(p1, p0, q1, q0),
    walsh1 = index_arithmetic("Walsh1")(p1, p0, q1, q0),
    vartia2 = index_geometric("Vartia2")(p1, p0, q1, q0),
    vartia1 = index_geometric("Vartia1")(p1, p0, q1, q0),
    stuval = index_stuval(2, 2)(p1, p0, q1, q0)
  )
}

round(t(mapply(index_formulas, price6, price6[1], quantity6, quantity6[1])), 4)
}