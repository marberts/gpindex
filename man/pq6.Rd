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
# Recreate table 3.6 from Balk (2008)

index_formulas <- function(p1, p0, q1, q0) {
  c(fisher = index_fisher(p1, p0, q1, q0),
    tornqvist = index_geometric(p1, p0, q1, q0, type = "Tornqvist"),
    marshall_edgeworth = index_arithmetic(p1, p0, q1, q0, type = "MarshallEdgeworth"),
    walsh1 = index_arithmetic(p1, p0, q1, q0, type = "Walsh1")
  )
}

round(t(mapply(index_formulas, price6, price6[1], quantity6, quantity6[1])), 4)

}


