\name{SIROC}
\alias{SIROC}
\title{SIROC -- Smoothed Indexed Rate Of Change}
\usage{
  SIROC(x, n1 = 30, n2 = 15, n3 = 14, ...)
}
\arguments{
  \item{n1}{a period upon which to compute an EMA based on
  price, and the lag for the normalizing price.}

  \item{n2}{a period for taking the EMA of the normalized
  residuals above}

  \item{n3}{an RSI period for the above quantity}

  \item{maType}{the moving average type used for the RSI.
  Defaults to EMA}
}
\value{
  the SIROC indicator, a 1-column oscillator
}
\description{
  computes the smoothed indexed rate of change indicator.
  The SIROC is an oscillator ranging between 0 and 100. It
  is computed as an n3-period RSI of an n2-period EMA of
  the normalized residuals of an n1-period EMA of the
  price.
}

