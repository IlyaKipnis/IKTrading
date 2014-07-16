#'Aggregate Maximum Excursion Charts
#'@param Portfolio the portfolio name (string)
#'@param Symbols the symbols for which to plot
#'@param type MAE for Maximum Adverse Excursion or MFE for Maximum Favorable Excursion
#'@param scale scale of P&L (cash, tick, percent)
#'@return plots all the trades among the symbols in one chart. Useful with equal risk order sizing.
#'@export
"agg.chart.ME" <- function(Portfolio, Symbols, type=c("MAE", "MFE"), scale=c("cash", "percent", "tick")) {
  type=type[1]
  scale=scale[1]
  trades <- list()
  length(trades) <- length(Symbols)
  for(Symbol in Symbols) {
    trades[[Symbol]] <- pts <- perTradeStats(Portfolio=Portfolio, Symbol=Symbol, includeOpenTrade=FALSE)
  }
  trades <- do.call(rbind, trades)
  trades$Pct.Net.Trading.PL <- 100 * trades$Pct.Net.Trading.PL
  trades$Pct.MAE <- 100 * trades$Pct.MAE
  trades$Pct.MFE <- 100 * trades$Pct.MFE
  profitable <- (trades$Net.Trading.PL > 0)
  switch(scale, cash = {
    .ylab <- "Profit/Loss (cash)"
    if (type == "MAE") {
      .cols <- c("MAE", "Net.Trading.PL")
      .xlab <- "Drawdown (cash)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("MFE", "Net.Trading.PL")
      .xlab <- "Run Up (cash)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  }, percent = {
    .ylab <- "Profit/Loss (%)"
    if (type == "MAE") {
      .cols <- c("Pct.MAE", "Pct.Net.Trading.PL")
      .xlab <- "Drawdown (%)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("Pct.MFE", "Pct.Net.Trading.PL")
      .xlab <- "Run Up (%)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  }, tick = {
    .ylab <- "Profit/Loss (ticks)"
    if (type == "MAE") {
      .cols <- c("tick.MAE", "tick.Net.Trading.PL")
      .xlab <- "Drawdown (ticks)"
      .main <- "Maximum Adverse Excursion (MAE)"
    } else {
      .cols <- c("tick.MFE", "tick.Net.Trading.PL")
      .xlab <- "Run Up (ticks)"
      .main <- "Maximum Favourable Excursion (MFE)"
    }
  })
  .main <- paste("All trades", .main)
  plot(abs(trades[, .cols]), type = "n", xlab = .xlab, ylab = .ylab, 
       main = .main)
  grid()
  points(abs(trades[profitable, .cols]), pch = 24, col = "green", 
         bg = "green", cex = 0.6)
  points(abs(trades[!profitable, .cols]), pch = 25, col = "red", 
         bg = "red", cex = 0.6)
  abline(a = 0, b = 1, lty = "dashed", col = "darkgrey")
  legend(x = "bottomright", inset = 0.1, legend = c("Profitable Trade", 
                                                    "Losing Trade"), pch = c(24, 25), col = c("green", "red"), 
         pt.bg = c("green", "red"))
}