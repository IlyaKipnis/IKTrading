\name{stratBoxPlots}
\alias{stratBoxPlots}
\title{Strategy Signal Expectancy Boxplots}
\usage{
  stratBoxPlots(strategy.st, symbols, from = NULL,
    to = NULL, short = FALSE, lagSeq = seq(1, 10),
    sigName = NULL, magicalThinking = FALSE,
    filenameMod = NULL, ...)
}
\arguments{
  \item{strategy.st}{the name of the strategy}

  \item{symbols}{a string of symbols for which there is
  OHLC data}

  \item{from}{a starting date (default NULL for complete
  data)}

  \item{to}{an ending date (default NULL for complete
  data)}

  \item{short}{whether the side of the trade is the long or
  short side}

  \item{lagSeq}{a sequence of days into the future.
  Defaults to 1 through 10}

  \item{sigName}{the name of the signal for which to create
  the box plots}

  \item{fileNameMod}{any name modifications for the title
  of the box plot and file name (EG parameter sets).}
}
\value{
  prints a box plot to a file
}
\description{
  Generates boxplots of percentage returns for a given
  signal over time.
}

