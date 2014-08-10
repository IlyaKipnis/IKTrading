\name{MSR}
\alias{MSR}
\title{Varadi's MSR}
\usage{
  MSR(HLC, nMed = 10, nMax = nMed * 2, pctLookBack = 100)
}
\arguments{
  \item{HLC}{an HLC xts}

  \item{nMed}{a lookback period for taking the median of
  the HLC series; i.e. the median of the concatenated HLC
  series, using a parameter of 3*nMed for the 3 series in
  the concatenation}

  \item{nMax}{a lookback period for the max of the HLC
  series -- should be greater than the median lookback}

  \item{pctLookBack}{a period over which to do a percent
  ranking}
}
\description{
  Computes David Varadi's MSR -- a percent rank of a
  normalized differnce of median and max
}
\references{
  \url{http://cssanalytics.wordpress.com/2010/10/27/a-new-trend-indicator-msr/}
}

