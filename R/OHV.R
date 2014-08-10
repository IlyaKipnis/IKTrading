#'Varadi's Offsetting HV
#'@description computes a ratio of an n1-day volatility to an n1-lagged n2-day volatility.
#'@param x a time series
#'@param n1 a lookback period for the first rolling standard deviation and lag for the second standard deviation
#'@param n2 a lookback period for the second standard deviation
#'@param sample whether or not to use a sample calculation or population calculation for standard deviation
#'@return a volatility ratio
#'@references \url{https://cssanalytics.wordpress.com/2010/11/18/310-offset-hv-as-a-mean-reversion-filter/}
#'@export
"OHV" <- function(x, n1=3, n2=10, sample=TRUE) {
  shortVol <- runSD(x, n=n1, sample=sample)
  midVol <- lag(runSD(x, n=n2, sample=sample), k=n1)
  OHV <- shortVol/midVol
  colnames(OHV) <- "OHV"
  return(OHV)
}
