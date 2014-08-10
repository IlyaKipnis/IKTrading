\name{DVO}
\alias{DVO}
\title{David Varadi Oscillator}
\usage{
  DVO(HLC, nAvg = 2, pctLookback = 126, maType = "SMA",
    deTrend = TRUE, nDT = 126)
}
\arguments{
  \item{HLC}{an HLC time series}

  \item{nAvg}{the number of days over which to average
  ratios}

  \item{pctLookback}{a percent ranking lookback period}

  \item{maType}{a function specifying the moving average
  type (defaults to SMA)}

  \item{deTrend}{whether or not to subtract an SMA of the
  indicator from itself}

  \item{nDT}{a lookback period for computing the moving
  average of the indicator to subtract from the center of
  the indicator, and then to subtract that quantity from
  the main computation}
}
\value{
  the DVO oscillator quantity, centered at 50
}
\description{
  Computes a percent ranking of an average of close over
  the sum of high and low values. The DV2 indicator uses an
  average period of 2.
}
\references{
  \url{https://cssanalytics.wordpress.com/2011/01/11/de-trending-indicators/}
  \cr
  \url{http://quantingdutchman.wordpress.com/2010/08/06/dv2-indicator-for-amibroker/}
}

