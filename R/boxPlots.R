#'Strategy Signal Expectancy Boxplots
#'@description Generates boxplots of percentage returns for a given signal over time.
#'@param strategy.st the name of the strategy
#'@param symbols a string of symbols for which there is OHLC data
#'@param from a starting date (default NULL for complete data)
#'@param to an ending date (default NULL for complete data)
#'@param short whether the side of the trade is the long or short side
#'@param lagSeq a sequence of days into the future. Defaults to 1 through 10
#'@param sigName the name of the signal for which to create the box plots
#'@param fileNameMod any name modifications for the title of the box plot and file name (EG parameter sets).
#'@return prints a box plot to a file
#'@export
"stratBoxPlots" <- function(strategy.st, symbols, from=NULL, to=NULL, short=FALSE,
                          lagSeq=seq(1,10), sigName=NULL, magicalThinking=FALSE, filenameMod=NULL, ...){
  for(symbol in symbols){
    OHLC <- OHLC(get(symbol))
    inds <- applyIndicators(strategy.st, OHLC)
    signals <- applySignals(strategy.st, inds)
    
    if(magicalThinking) {
      lagData <- lag(Cl(OHLC), lagSeq*-1)
      mat <- matrix(nrow=nrow(lagData),ncol=length(lagSeq),rep(Cl(OHLC), length(lagSeq)))
      pctDiffs <- (lagData-mat)/mat*(1-2*short)
    } else {
      nextDayOps <- lag(Op(OHLC), -1)
      mat <- matrix(nrow=nrow(nextDayOps),ncol=length(lagSeq), rep(nextDayOps, length(lagSeq)))
      lagData <- lag(nextDayOps, lagSeq*-1)
      pctDiffs <- (lagData-mat)/mat*(1-2*short)
    }
    colnames(pctDiffs) <- NULL
    dataAndDiffs <- cbind(signals, pctDiffs)
    sigNameCol <- which(colnames(dataAndDiffs)==sigName)
    sigDays <- dataAndDiffs[dataAndDiffs[,sigNameCol]==1,]
    pctDiffs <- pctDiffs[index(sigDays),]
    if(!is.null(from) | !is.null(to)) {
      dateString <- paste(from,to,sep="::")
      pctDiffs <- pctDiffs[dateString]
    }
    colnames(pctDiffs) <- paste0("day",lagSeq)
    pctDiffs <- data.frame(pctDiffs)
    jpeg(paste0(strategy.st,"_",symbol,"_",filenameMod,".jpeg"))
    boxplot(pctDiffs, main=paste(strategy.st, symbol, filenameMod,sep="_"), ylab="Returns", xlab="Days Ahead", 
            staplecex=0.5, outcex=0.7)
    points(colMeans(pctDiffs), col="cyan")
    abline(h=0, col="red", lwd=3)
    dev.off()
  }
}