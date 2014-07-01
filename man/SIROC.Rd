\name{SIROC}
\alias{SIROC}
\title{SIROC -- Smoothed Indexed Rate Of Change}
\usage{
  SIROC(x, nEMA1 = 30, nEMA2 = 15, nEMA3 = 14)
}
\arguments{
  \item{nEMA1}{a period upon which to compute an EMA based
  on price, and the normalizing lag.}

  \item{nEMA2}{a period for taking the EMA of
  (price-EMA1)/lag(EMA1, nEMA1)}

  \item{nEMA3}{a period for taking the ratio of differences
  of EMAs of the above EMA}
}
\value{
  the SIROC indicator, a 1-column oscillator
}
\description{
  computes the smoothed indexed rate of change indicator.
  The SIROC is an oscillator ranging between 0 and 1. It is
  computed thus: Take an nEMA1 period EMA of the price, and
  subtract it from the price. Then, divide that quantity by
  the EMA nEMA1 periods ago. Next, take an nEMA2 EMA of
  that quantity, and take its difference. Finally, take the
  ratio of the nEMA3 EMA of the positive differences to the
  total of the EMAs of the positive and absolute value of
  the negative difference EMAs.
}

