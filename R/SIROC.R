#'SIROC -- Smoothed Indexed Rate Of Change
#'@description computes the smoothed indexed rate of change indicator.
#'The SIROC is an oscillator ranging between 0 and 1. It is computed thus:
#'Take an nEMA1 period EMA of the price, and subtract it from the price. Then,
#'divide that quantity by the EMA nEMA1 periods ago. Next, take an nEMA2 EMA
#'of that quantity, and take its difference. Finally, take the ratio of the nEMA3
#'EMA of the positive differences to the total of the EMAs of the positive and absolute value
#'of the negative difference EMAs.
#'@param nEMA1 a period upon which to compute an EMA based on price, and the normalizing lag.
#'@param nEMA2 a period for taking the EMA of (price-EMA1)/lag(EMA1, nEMA1)
#'@param nEMA3 a period for taking the ratio of differences of EMAs of the above EMA
#'@return the SIROC indicator, a 1-column oscillator
#'@export
"SIROC" <- function(x, nEMA1=30, nEMA2=15, nEMA3=14) {
  ema1 <- EMA(x, n=nEMA1)
  tmp1 <- (x-ema1)/lag(ema1, k=nEMA1)
  ema2 <- EMA(tmp1, n=nEMA2)
  diffEma2 <- diff(ema2)
  posDiff <- diffEma2 * (diffEma2 > 0)
  negDiff <- abs(diffEma2 * (diffEma2 < 0))
  posEMA <- EMA(posDiff, n=nEMA3)
  negEMA <- EMA(negDiff, n=nEMA3)
  SIROC <- posEMA/(posEMA+negEMA)
  colnames(SIROC) <- "SIROC"
  return(SIROC)
}