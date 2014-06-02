#'Lagged ATR
#'@description lags ATR computation by a lag parameter for use with order-sizing functions
#'@param HLC an HLC object
#'@param n a lookback period
#'@param maType the type of moving average
#'@param lag how many periods to lag the computation
#'@return a lagged ATR calculation
#'@export
"lagATR" <- function(HLC, n=14, maType, lag=1, ...) {
  ATR <- ATR(HLC, n=n, maType=maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}

#'osDollarATR
#'@description computes an order size by way of ATR quantities, as a proportion of tradeSize
#'@param orderside long or short
#'@param tradeSize a notional dollar amount for the trade
#'@param pctATR a percentage of the tradeSize to order in units of ATR. That is, if tradeSize is
#'10000 and pctATR is .02, then the amount ordered will be 200 ATRs of the security.
#'If the last observed ATR is 2, then 100 units of the security will be ordered.
#'@param maxPctATR an upper limit to how many ATRs can be held in a position; a risk limit
#'@param integerQty an integer quantity of shares
#'@param atrMod a string modifier in case of multiples of this indicator being used.
#'Will append to the term 'atr', that is, atrMod of "X" will search for a term called 'atrX'
#'in the column names of the mktdata xts object.
#'@param rebal if TRUE, and current position exceeds ATR boundaries, will automatically sell
#'@export
"osDollarATR" <- function(orderside, tradeSize, pctATR, maxPctATR=pctATR, data, timestamp, symbol,
                        prefer="Open", portfolio, integerQty=TRUE, atrMod="", rebal=FALSE, ...) {
  if(tradeSize > 0 & orderside == "short"){
    tradeSize <- tradeSize*-1
  }
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0("atr",atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  if(length(atrCol)==0) {
    stop(paste("Term", atrString, "not found in mktdata column names."))
  }
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if(is.na(atrTimeStamp) | atrTimeStamp==0) {
    stop(paste("ATR corresponding to",atrString,"is invalid at this point in time. 
               Add a logical operator to account for this."))
  }
  dollarATR <- pos*atrTimeStamp
  desiredDollarATR <- pctATR*tradeSize
  remainingRiskCapacity <- tradeSize*maxPctATR-dollarATR
  
  if(orderside == "long"){
    qty <- min(tradeSize*pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  } else {
    qty <- max(tradeSize*pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  }
  
  if(integerQty) {
    qty <- trunc(qty)
  }
  if(!rebal) {
    if(orderside == "long" & qty < 0) {
      qty <- 0
    }
    if(orderside == "short" & qty > 0) {
      qty <- 0
    }
  }
  if(rebal) {
    if(pos == 0) {
      qty <- 0
    }
  }
  return(qty)
}