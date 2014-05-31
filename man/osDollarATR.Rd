\name{osDollarATR}
\alias{osDollarATR}
\title{osDollarATR}
\usage{
  osDollarATR(orderside, tradeSize, pctATR,
    maxPctATR = pctATR, data, timestamp, symbol,
    prefer = "Open", portfolio, integerQty = TRUE,
    atrMod = "", rebal = FALSE, ...)
}
\arguments{
  \item{orderside}{long or short}

  \item{tradeSize}{a notional dollar amount for the trade}

  \item{pctATR}{a percentage of the tradeSize to order in
  units of ATR. That is, if tradeSize is 10000 and pctATR
  is .02, then the amount ordered will be 200 ATRs of the
  security. If the last observed ATR is 2, then 100 units
  of the security will be ordered.}

  \item{maxPctATR}{an upper limit to how many ATRs can be
  held in a position; a risk limit}

  \item{integerQty}{an integer quantity of shares}

  \item{atrMod}{a string modifier in case of multiples of
  this indicator being used. Will append to the term 'atr',
  that is, atrMod of "X" will search for a term called
  'atrX' in the column names of the mktdata xts object.}

  \item{rebal}{if TRUE, and current position exceeds ATR
  boundaries, will automatically sell}
}
\description{
  computes an order size by way of ATR quantities, as a
  proportion of tradeSize
}

