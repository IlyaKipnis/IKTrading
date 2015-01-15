\name{EAA}
\alias{EAA}
\title{Elastic Asset Allocation (EAA)}
\usage{
  EAA(monthlyPrices, wR = 1, wV = 0, wC = 0.5, wS = 2,
    errorJitter = 1e-06, cashAsset = NULL,
    bestN = 1 + ceiling(sqrt(ncol(monthlyPrices))),
    enableCrashProtection = TRUE, returnWeights = FALSE,
    monthlyRiskFree = NULL)
}
\arguments{
  \item{monthlyPrices}{a price series using monthly data}

  \item{wR}{the weight to place on returns (default 1)}

  \item{wV}{the weight for volatility (default 0)}

  \item{wC}{the weight for correlations (default .5)}

  \item{wS}{a selection aggressiveness weight. A weight of
  infinity will result in a single security. A weight of 0
  will result in near equal weights of the selected
  securities. (default 2)}

  \item{errorJitter}{a small error term due to volatility
  in the denomiantor (default 1e-6)}

  \item{cashAsset}{the name (string) of the asset to be
  used as the cash asset. If NULL, will result in zeroes.
  (default NULL)}

  \item{bestN}{the number of securities to select every
  period (default 1 + square root of number of assets in
  universe)}

  \item{enableCrashProtection}{if enabled, will result in
  weights being multiplied by 1-n/(size of universe), where
  n is the number of securities with negative absolute
  returns (default TRUE)}

  \item{returnWeights}{whether or not to return the
  portfolio weights along with the returns in a list format
  (default FALSE)}

  \item{monthlyRiskFree}{if provided, a time series of
  prices of a risk-free asset (E.G.: IRX), for use in
  subtracting from asset returns to compute asset returns
  (default NULL)}
}
\value{
  a monthly xts of the strategy's returns
}
\description{
  Asset allocation algorithm from Keller (2014-12-30). EAA
  performs asset selection and weighting based on weights
  for returns, volatility, and correlation to the
  equally-weighted universe.
}
\references{
  \url{https://quantstrattrader.wordpress.com/2015/01/03/for-a-new-year-a-new-asset-allocation-system-just-published-in-ssrn/}
  \cr
  \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979}
}

