\name{sigAND}
\alias{sigAND}
\title{sigAND}
\usage{
  sigAND(label, data = mktdata, columns, cross = FALSE)
}
\arguments{
  \item{label}{name of the output signal}

  \item{data}{the market data}

  \item{columns}{the signal columns to intersect}

  \item{cross}{whether to only provide a true value for
  crossing values}
}
\value{
  a new signal column that intersects the provided columns
}
\description{
  signal AND operator for quantstrat signals.
}

