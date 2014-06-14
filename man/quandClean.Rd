\name{quandClean}
\alias{quandClean}
\title{Quandl Futures Data Cleaning}
\usage{
  quandClean(stemCode, start_date = NULL, end_date = NULL,
    verbose = FALSE, ...)
}
\arguments{
  \item{stemCode}{the characters for the corresponding
  futures series. Usually in the form of
  CHRIS/EXCHANGE_SYMBOL, such as CHRIS/CME_CL for light,
  sweet crude oil, and CHRIS/CME_GC for gold.}

  \item{start_date}{a character string date in the form of
  yyyy-mm-dd such as 2000-01-01}

  \item{end_date}{a character string date identical to the
  above}

  \item{verbose}{an argument that displays how many NA and
  spike days were removed, then prints the removed dates}
}
\value{
  an OHLCVI time series of daily data
}
\description{
  Constructs a continuous futures time series from front
  and back month contracts from Quandl's database.
}

