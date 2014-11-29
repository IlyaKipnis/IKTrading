\name{FAA}
\alias{FAA}
\title{Flexible Asset Allocation returns algorithm}
\usage{
  FAA(prices, monthLookback = 4, weightMom = 1,
    weightVol = 0.5, weightCor = 0.5, riskFreeName = NULL,
    bestN = 3, stepCorRank = FALSE,
    stepStartMethod = c("best", "default"),
    geometric = TRUE, riskFreeInTie = TRUE,
    returnWeights = FALSE, ...)
}
\arguments{
  \item{prices}{a price history for assets intended to be
  traded over the course of the simulation. Need not be
  contemporaneous.}

  \item{monthLookback}{a monthly lookback period over which
  to compute momentum, volatility, and correlations.
  (Default 4)}

  \item{weightMom}{the weight to put on the momentum rank
  in the calculation weightMom*rankMom + weightVol*rankVol
  + weightCor*rankCor (default 1)}

  \item{weightVol}{the weight of the volatility rank in the
  above equation (default .5)}

  \item{weightCor}{the weight of the correlation rank in
  the aforementioned equation (default .5)}

  \item{riskFreeName}{name of the security to be used as
  the risk free rate. If NULL, will use a vector of zeroes.
  (Default NULL) Recommended to use
  quandClean('CHRIS/CME_US'), SHY, or VFISX}

  \item{bestN}{the top N securities to invest in. Will
  invest in more than bestN in the event of a tie for the
  lowest bestN.}

  \item{stepCorRank}{whether or not to use the stepwise
  correlation rank (see stepwiseCorRank function). If
  FALSE, ranks correlations based on sums of correlations
  of each asset to all other assets. (Default FALSE)}

  \item{stepStartMethod}{the method to determine the
  starting asset for the stepwise correlation rank
  algorithm, if enabled. The 'best' argument chooses the
  best ranked asset by the momentum and volatility weights,
  while the 'default' method starts with the initial
  lowest-correlated asset. (Default 'best')}

  \item{geometric}{whether or not to use geometric
  compounding for returns (default TRUE)}

  \item{riskFreeInTie}{whether to allocate to the risk-free
  asset in the event of a tie (default TRUE)}

  \item{returnWeights}{if TRUE, returns a length-two list
  of weights and returns (default FALSE)}
}
\value{
  a single xts of strategy returns
}
\description{
  implements the Flexible Asset Allocation algorithm, by
  Keller and Van Putten (2012). FAA takes the best assets
  as ranked by a weighted rank-sum of momentum, volatility,
  and correlation. This implementation first filters on
  momentum above zero after performing all ranking and
  weight allocaations.
}
\references{
  \url{http://quantstrattrader.wordpress.com/2014/10/31/combining-faa-and-stepwise-correlation/}
  \cr
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735}
}

