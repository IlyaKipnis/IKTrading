#'Hourly P&L box plot
#'@param Portfolio the portfolio name
#'@param symbol the symbol string
#'@return a box plot of the hourly P&Ls
#'@export
"hourlyPL" <- function(Portfolio, symbol) {
  pname <- Portfolio
  Portfolio <- get(paste("portfolio", pname, sep="."), envir=.blotter)
  txn <- Portfolio$symbols[[symbol]]$txn
  posPL <- Portfolio$symbols[[symbol]]$posPL
  posPL <- posPL[-1, ]
  PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]
  if (length(PL.ne0) == 0) {
    next
  }
  PL.ne0$hours <- strftime(index(PL.ne0), "%H")
  boxplot(as.numeric(PL.ne0$Net.Txn.Realized.PL)~PL.ne0$hours)
  abline(h=0, col="red")
}
