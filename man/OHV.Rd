\name{OHV}
\alias{OHV}
\title{Varadi's Offsetting HV}
\usage{
  OHV(x, n1 = 3, n2 = 10, sample = TRUE)
}
\arguments{
  \item{x}{a time series}

  \item{n1}{a lookback period for the first rolling
  standard deviation and lag for the second standard
  deviation}

  \item{n2}{a lookback period for the second standard
  deviation}

  \item{sample}{whether or not to use a sample calculation
  or population calculation for standard deviation}
}
\value{
  a volatility ratio
}
\description{
  computes a ratio of an n1-day volatility to an n1-lagged
  n2-day volatility.
}
\references{
  \url{https://cssanalytics.wordpress.com/2010/11/18/310-offset-hv-as-a-mean-reversion-filter/}
}

