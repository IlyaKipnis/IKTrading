#'Order Size: Max Dollar
#'@description An order sizing function that limits position size based on dollar value of the position,
#'rather than quantity of shares.
#'@param tradeSize the dollar value to transact (use negative number to sell short)
#'@param maxSize the dollar limit to the position (use negative number for short side)
#'@param integerQty a boolean whether or not to truncate to the nearest integer of contracts/shares/etc.
#'@return a quantity to order
#'@export
"osMaxDollar" <- function(data, timestamp, orderqty, ordertype, orderside,
                        portfolio, symbol, prefer="Open", tradeSize,
                        maxSize, integerQty=TRUE,
                        ...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  if(prefer=="Close") {
    price <- as.numeric(Cl(mktdata[timestamp,]))
  } else {
    price <- as.numeric(Op(mktdata[timestamp,]))
  }
  posVal <- pos*price
  if (orderside=="short") {
    dollarsToTransact <- max(tradeSize, maxSize-posVal)
    #If our position is profitable, we don't want to cover needlessly.
    if(dollarsToTransact > 0) {dollarsToTransact=0}
  } else {
    dollarsToTransact <- min(tradeSize, maxSize-posVal)
    #If our position is profitable, we don't want to sell needlessly.
    if(dollarsToTransact < 0) {dollarsToTransact=0}
  }
  qty <- dollarsToTransact/price
  if(integerQty) {
    qty <- trunc(qty)
  }
  return(qty)
}