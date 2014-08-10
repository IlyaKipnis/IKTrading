#'David Varadi Oscillator
#'@description Computes a percent ranking of an average of close 
#'over the sum of high and low values. The DV2 indicator uses an average period of 2.
#'@param HLC an HLC time series
#'@param nAvg the number of days over which to average ratios
#'@param pctLookback a percent ranking lookback period
#'@param maType a function specifying the moving average type (defaults to SMA)
#'@param deTrend whether or not to subtract an SMA of the indicator from itself
#'@param nDT a lookback period for computing the moving average of the 
#'indicator to subtract from the center of the indicator, and then to subtract that quantity
#'from the main computation
#'@return the DVO oscillator quantity, centered at 50
#'@references \url{https://cssanalytics.wordpress.com/2011/01/11/de-trending-indicators/}
#'\cr \url{http://quantingdutchman.wordpress.com/2010/08/06/dv2-indicator-for-amibroker/}
#'@export
"DVO" <- function(HLC, nAvg=2, pctLookback=126, 
                  maType="SMA", deTrend=TRUE, 
                  nDT=126) {
  maType <- match.fun(maType)
  ratio <- Cl(HLC)/(Hi(HLC)+Lo(HLC))
  avgRatio <- maType(ratio, n=nAvg)
  DVO <- runPercentRank(avgRatio, n=pctLookback, exact.multiplier=1)*100
  if(deTrend) {
    DVO <- DVO + 50 - SMA(DVO, nDT)
  }
  colnames(DVO) <- "DVO"
  return(DVO)
}
