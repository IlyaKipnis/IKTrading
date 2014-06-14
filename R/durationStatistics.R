#'Duration Statistics
#'@description a collection of basic statistics on durations of trades--most freely available data will be on daily frequency
#'@param Portfolio -- the portfolio name
#'@param Symbols -- the names of the symbols in the backtest
#'@param includeOpenTrade -- whether to include open trades -- defaults to FALSE since if a trade opens on the same day as the last day, units will be off
#'@return the min, Q1, median, mean, Q3, and max durations of all trades, winning trades (W), and losing trades (L)
#'@export
"durationStatistics" <- function(Portfolio, Symbols, includeOpenTrade=FALSE, ...) {
  tmp <- list()
  length(tmp) <- length(Symbols)
  for(Symbol in Symbols) {
    pts <- perTradeStats(Portfolio=Portfolio, Symbol=Symbol, includeOpenTrade=includeOpenTrade)
    pts$diff <- pts$End-pts$Start
    durationSummary <- summary(as.numeric(pts$diff))
    winDurationSummary <- summary(as.numeric(pts$diff[pts$Net.Trading.PL > 0]))
    lossDurationSummary <- summary(as.numeric(pts$diff[pts$Net.Trading.PL <= 0]))
    names(durationSummary) <- names(winDurationSummary) <- names(lossDurationSummary) <- c("Min","Q1","Med", "Mean","Q3","Max")
    names(winDurationSummary) <- paste0("W", names(winDurationSummary))
    names(lossDurationSummary) <- paste0("L", names(lossDurationSummary))
    dataRow <- data.frame(cbind(t(round(durationSummary)), t(round(winDurationSummary)), t(round(lossDurationSummary))))
    tmp[[Symbol]] <- dataRow
  }
  out <- do.call(rbind, tmp)
  return(out)
}