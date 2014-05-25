\name{ichimoku}
\alias{ichimoku}
\title{Ichimoku}
\usage{
  ichimoku(HLC, nFast = 9, nMed = 26, nSlow = 52)
}
\arguments{
  \item{HLC}{an HLC time series}

  \item{nFast}{a fast period of days, default 9}

  \item{nMed}{a medium period of days, default 26}

  \item{nSlow}{a slow period of days, default 52}
}
\value{
  The first four computations (turning line, base line,
  span A, span B), plotSpan (do NOT use this for
  backtesting, but for plotting), laggingSpan, and a lagged
  Span A and lagged Span B for comparisons with the lagging
  span, as per Ichimoku strategies.
}
\description{
  The ichimoku indicator, as invented by Goichi Hosoda. It
  has five components. \cr The turning line is the average
  of the highest high and highest low of the past nFast
  periods. \cr The base line is computed the same way over
  the course of nMed periods. \cr Span A is the average of
  the above two calculations, projected nMed periods into
  the future. \cr Span B is the average of the highest high
  and lowest low over the past nSlow periods, also
  projected the same way. \cr Finally, the lagging span is
  the close, projected backwards by nMed periods.
}

