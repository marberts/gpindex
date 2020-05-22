\docType{data}
\name{food prices}
\alias{food_prices}

\title{Quarterly prices for NAICS 311}

\description{
Randomly generated prices from manufacturers in NAICS 311 from Q1 2020 to Q4 2020.}

\usage{
food_prices
}

\format{
A dataframe with 5 columns and 1,905 rows.
\describe{
\item{pid}{A unique product identifier (integer).}
\item{quarter}{The reference quarter (date).}
\item{price_current}{The price in the current reference quarter (numeric).}
\item{price_previous}{The price in the previous quarter (numeric).}
\item{naics}{The 6-digit NAICS code for the product (character).}
}
}
