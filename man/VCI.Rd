\name{VCI}
\alias{VCI}
\title{ValueCharts Indicator}
\usage{
  VCI(OHLC, nLookback = 40, nRange = 8, pctRank = FALSE)
}
\arguments{
  \item{OHLC}{an OHLC time series}

  \item{nLookback}{a moving average window, default 40}

  \item{nRange}{a lookback window for the range
  computation. A number above 7 results in the following
  computation: take the difference between the nRange max
  high and min low. Add up that value, that value lagged by
  nRange+1, nRange*2, nRange*3, and nRange*4, and divide by
  25. If nRange is 7 or less, the daily range is instead
  computed as the 5-day SMA of the following quantity: for
  each day, take the maximum of the difference between the
  high and low, or the absolute difference between current
  and previous close. Multiply that final quantity by .16.}
}
\value{
  an OHLC-like 4-column output computing the values given
  OHLC prices, or a percent rank of the close variant. The
  column names will be VO, VH, VL, and VC, respectively.
}
\description{
  Computes a range-normalized difference between current
  price and a running moving average. A value below -8
  indicates oversold, while a value above 8 indicates
  overbought.
}
\references{
  \url{http://www.tradesignalonline.com/en/lexicon/view.aspx?id=Value+Charts+Indicator}
}

