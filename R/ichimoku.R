#'Ichimoku
#'@description The ichimoku indicator, as invented by Goichi Hosoda. It has five components. 
#'\cr The turning line is the average of the highest high and highest low of the past nFast periods.
#'\cr The base line is computed the same way over the course of nMed periods.
#'\cr Span A is the average of the above two calculations, projected nMed periods into the future.
#'\cr Span B is the average of the highest high and lowest low over the past nSlow periods, also projected the same way.
#'\cr Finally, the lagging span is the close, projected backwards by nMed periods.
#'@param HLC an HLC time series
#'@param nFast a fast period of days, default 9
#'@param nMed a medium period of days, default 26
#'@param nSlow a slow period of days, default 52
#'@return The first four computations (turning line, base line, span A, span B), plotSpan (do NOT use this for backtesting, but for plotting),
#'laggingSpan, and a lagged Span A and lagged Span B for comparisons with the lagging span, as per Ichimoku strategies.
#'@export
"ichimoku" <- function(HLC, nFast=9, nMed=26, nSlow=52) {
  turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
  baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
  spanA <- lag((turningLine+baseLine)/2, nMed)
  spanB <- lag((runMax(Hi(HLC), nSlow)+runMin(Lo(HLC), nSlow))/2, nMed)
  plotSpan <- lag(Cl(HLC), -nMed) #for plotting the original Ichimoku only
  laggingSpan <- lag(Cl(HLC), nMed)
  lagSpanA <- lag(spanA, nMed)
  lagSpanB <- lag(spanB, nMed)
  out <- cbind(turnLine=turningLine, baseLine=baseLine, spanA=spanA, spanB=spanB, plotSpan=plotSpan, laggingSpan=laggingSpan, lagSpanA, lagSpanB)
  colnames(out) <- c("turnLine", "baseLine", "spanA", "spanB", "plotLagSpan", "laggingSpan", "lagSpanA","lagSpanB")
  return (out)
}