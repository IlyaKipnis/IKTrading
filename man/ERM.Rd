\name{ERM}
\alias{ERM}
\title{Varadi's Error-Adjusted Momentum}
\usage{
  ERM(x, nFcast = 10, nMAE = 10, nAvg = 200,
    maType = "SMA")
}
\arguments{
  \item{nFcast}{lookback parameter for a short term
  forecast (default 10)}

  \item{nMAE}{lookback parameter for a mean absolute error
  term (default 10)}

  \item{nAvg}{lookback parameter for the moving average
  (default 200)}

  \item{maType}{string indicating moving average type
  (default "SMA")}
}
\value{
  the Varadi Error-Adjusted Momentum series
}
\description{
  Computes a zero-centered indicator about zero that is a
  moving average of the absolute residuals of a short-term
  forecast of the returns of a price series.
}
\references{
  \url{http://cssanalytics.wordpress.com/2014/07/30/error-adjusted-momentum/}
}

