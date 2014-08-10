#'Varadi's Error-Adjusted Momentum
#'@description Computes a zero-centered indicator about zero that is a 
#'moving average of the absolute residuals of a short-term forecast of the returns of a price series.
#'@param nFcast lookback parameter for a short term forecast (default 10)
#'@param nMAE lookback parameter for a mean absolute error term (default 10)
#'@param nAvg lookback parameter for the moving average (default 200)
#'@param maType string indicating moving average type (default "SMA")
#'@return the Varadi Error-Adjusted Momentum series
#'@references \url{http://cssanalytics.wordpress.com/2014/07/30/error-adjusted-momentum/}
#'@export
"ERM" <- function(x, nFcast=10, nMAE=10, nAvg=200, maType="SMA") {
  maType <- match.fun(maType)
  rets <- diff(log(x))
  fcast <- maType(rets, n=nFcast)
  absResids <- abs(rets-fcast)
  MAE <- maType(absResids, n=nMAE)
  normalizedRets <- rets/MAE
  errorMomentum <- maType(normalizedRets, n=nAvg)
  colnames(errorMomentum) <- "ERM"
  return(errorMomentum)
}