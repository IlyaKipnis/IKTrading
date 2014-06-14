\name{durationStatistics}
\alias{durationStatistics}
\title{Duration Statistics}
\usage{
  durationStatistics(Portfolio, Symbols,
    includeOpenTrade = FALSE, ...)
}
\arguments{
  \item{Portfolio}{-- the portfolio name}

  \item{Symbols}{-- the names of the symbols in the
  backtest}

  \item{includeOpenTrade}{-- whether to include open trades
  -- defaults to FALSE since if a trade opens on the same
  day as the last day, units will be off}
}
\value{
  the min, Q1, median, mean, Q3, and max durations of all
  trades, winning trades (W), and losing trades (L)
}
\description{
  a collection of basic statistics on durations of
  trades--most freely available data will be on daily
  frequency
}

