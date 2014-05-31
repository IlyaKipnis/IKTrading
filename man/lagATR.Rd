\name{lagATR}
\alias{lagATR}
\title{Lagged ATR}
\usage{
  lagATR(HLC, n = 14, maType, lag = 1, ...)
}
\arguments{
  \item{HLC}{an HLC object}

  \item{n}{a lookback period}

  \item{maType}{the type of moving average}

  \item{lag}{how many periods to lag the computation}
}
\value{
  a lagged ATR calculation
}
\description{
  lags ATR computation by a lag parameter for use with
  order-sizing functions
}

