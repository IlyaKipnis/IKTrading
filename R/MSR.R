#'Varadi's MSR
#'@description Computes David Varadi's MSR -- a percent rank of a normalized differnce of median and max
#'@param HLC an HLC xts
#'@param nMed a lookback period for taking the median of the HLC series;
#'i.e. the median of the concatenated HLC series, using a parameter of 3*nMed for the 3 series in the concatenation
#'@param nMax a lookback period for the max of the HLC series -- should be greater than the median lookback
#'@param pctLookBack a period over which to do a percent ranking
#'@references \url{http://cssanalytics.wordpress.com/2010/10/27/a-new-trend-indicator-msr/}
#'@export
"MSR" <- function(HLC, nMed=10, nMax=nMed*2, pctLookBack=100) {
  HLConeSeries <- rbind(Hi(HLC), Lo(HLC), Cl(HLC))
  HLCrunMed <- runMedian(HLConeSeries, n=nMed*3)
  ones <- xts(rep(1,length(HLConeSeries)), order.by=index(HLConeSeries))
  tmp <- cbind(HLCrunMed, ones)
  colnames(tmp) <- c("med", "ones")
  tmp$cumSum <- cumsum(tmp$ones)
  med <- tmp$med[tmp$cumSum %% 3==0,]
  runmax <- runMax(Hi(HLC), nMax)
  tmp <- (med-runmax)/runmax
  out <- runPercentRank(tmp, n=pctLookBack, exact.multiplier=1)
  colnames(out) <- "MSR"
  out
}