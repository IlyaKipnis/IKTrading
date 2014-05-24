#'HeikinAshi
#'@param OHLC an OHLC time series
#'@return the Heikin Ashi recomputed OHLC time series
#'@export
"heikinAshi" <- function(OHLC) {
  heikinAshi <- xts(matrix(nrow=dim(OHLC)[1], ncol=dim(OHLC)[2],0), order.by=index(OHLC))
  colnames(heikinAshi) <- c("xO","xH","xL","xC")
  heikinAshi$xC[1] <- (Op(OHLC)[1]+Cl(OHLC)[1])/2
  heikinAshi$xO <- rowMeans(OHLC)
  heikinAshi$xC <- computeHaClose(heikinAshi$xO, heikinAshi$xC)
  
  tmp <- cbind(Hi(OHLC),Lo(OHLC),heikinAshi$xO,heikinAshi$xC)
  heikinAshi$xH <- apply(tmp,1,max)
  heikinAshi$xL <- apply(tmp,1,min)
  heikinAshi$posNeg <- 1
  heikinAshi$posNeg[heikinAshi$C < heikinAshi$O] <- -1
  return(heikinAshi)
}

