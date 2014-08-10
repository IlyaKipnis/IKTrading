#'SIROC -- Smoothed Indexed Rate Of Change
#'@description computes the smoothed indexed rate of change indicator.
#'The SIROC is an oscillator ranging between 0 and 100. It is computed as an n3-period RSI of
#'an n2-period EMA of the normalized residuals of an n1-period EMA of the price.
#'@param n1 a period upon which to compute an EMA based on price, and the lag for the normalizing price.
#'@param n2 a period for taking the EMA of the normalized residuals above
#'@param n3 an RSI period for the above quantity
#'@param maType the moving average type used for the RSI. Defaults to EMA
#'@return the SIROC indicator, a 1-column oscillator
#'@export
"SIROC" <- function(x, n1=30, n2=15, n3=14, ...) {
  ema1 <- EMA(x, n=n1)
  tmp1 <- (x-ema1)/lag(ema1, k=n1)
  ema2 <- EMA(tmp1, n=n2)
  SIROC <- RSI(ema2, n=n3, maType=maType, ...)
  colnames(SIROC) <- "SIROC"
  return(SIROC)
}
