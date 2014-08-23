#'ValueCharts Indicator
#'@description Computes a range-normalized difference between current price and a running moving average. 
#'A value below -8 indicates oversold, while a value above 8 indicates overbought.
#'@param OHLC an OHLC time series
#'@param nLookback a moving average window, default 40
#'@param nRange a lookback window for the range computation. A number above
#'7 results in the following computation: take the difference between the nRange
#'max high and min low. Add up that value, that value lagged by nRange+1, nRange*2, nRange*3, and nRange*4,
#'and divide by 25.
#'If nRange is 7 or less, the daily range is instead computed as the 5-day SMA of the following quantity:
#'for each day, take the maximum of the difference between the high and low, or the absolute difference between current and previous close.
#'Multiply that final quantity by .16.
#'@return an OHLC-like 4-column output computing the values given OHLC prices, or a percent rank of the close variant.
#'The column names will be VO, VH, VL, and VC, respectively.
#'@references \url{http://www.tradesignalonline.com/en/lexicon/view.aspx?id=Value+Charts+Indicator}
#'@export
"VCI" <- function(OHLC, nLookback=40, nRange=8, pctRank=FALSE) {
  if(nLookback > 7) {
    varA <- runMax(Hi(OHLC), nRange) - runMin(Lo(OHLC), nRange)
    varB <- lag(varA, nRange+1)
    varC <- lag(varA, nRange*2)
    varD <- lag(varA, nRange*3)
    varE <- lag(varA, nRange*4)
    LRange <- (varA+varB+varC+varD+varE)/25    
  }
  if(nLookback <=7) {
    absDiff <- abs(diff(Cl(OHLC)))
    dailyRange <- Hi(OHLC) - Lo(OHLC)
    tmp <- cbind(absDiff, dailyRange)
    maxTmp <- pmax(tmp)
    LRange <- SMA(maxTmp, 5)*.16
  }
  hilo <- (Hi(OHLC)+Lo(OHLC))/2
  VO <- (Op(OHLC)-SMA(hilo, nLookback))/LRange
  VH <- (Hi(OHLC)-SMA(hilo, nLookback))/LRange
  VL <- (Lo(OHLC)-SMA(hilo, nLookback))/LRange
  VC <- (Cl(OHLC)-SMA(hilo, nLookback))/LRange
  out <- cbind(VO=VO, VH=VH, VL=VL, VC=VC)
  colnames(out) <- c("VO", "VH", "VL", "VC")
  return(out)
}