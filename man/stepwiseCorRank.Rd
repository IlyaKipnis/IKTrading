\name{stepwiseCorRank}
\alias{stepwiseCorRank}
\title{Stepwise Correlation Rank}
\usage{
  stepwiseCorRank(corMatrix, startNames = NULL,
    stepSize = 1, bestHighestRank = FALSE)
}
\arguments{
  \item{corMatrix}{a correlation matrix}

  \item{startNames}{names of the vectors to start initial
  ranking from. If NULL, the stepSize amount of vectors
  with the lowest overall correlation will be the initial
  vectors to start the process. Default NULL}

  \item{stepSize}{how many vectors per ranking step (EG
  stepSize of 2 adds the 2 lowest correlation-ranked
  vectors to existing set at each step). Default 1}

  \item{bestHighestRank}{whether or not to assign the
  highest rank (number of vectors) to the
  minimum-correlated vector(s). TRUE variant used in
  Flexible Asset Allocation (FAA). Default FALSE}
}
\value{
  a set of ranks for the given names of the vectors in the
  correlation matrix
}
\description{
  computes a stepwise correlation ranking of vectors
  starting from a given subset of vectors
}
\references{
  \url{http://quantstrattrader.wordpress.com/2014/10/27/introducing-stepwise-correlation-rank/}
  \cr
  \url{http://cssanalytics.wordpress.com/2014/10/27/flexible-asset-allocation-with-conditional-correlations/}
}

