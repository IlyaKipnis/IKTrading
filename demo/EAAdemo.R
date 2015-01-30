require(IKTrading)
require(quantmod)
require(PerformanceAnalytics)

symbols <- c("VTSMX", "FDIVX", "VEIEX", "VBMFX", "VFISX", "VGSIX", "QRAAX")

getSymbols(symbols, from="1990-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
ep <- endpoints(prices, "months")
prices <- prices[ep,]
prices <- prices["1997-03::"]

getSymbols("^IRX", from="1990-01-01")
dailyYield <- (1+(Cl(IRX)/100))^(1/252) - 1
threeMoPrice <- cumprod(1+dailyYield)
threeMoPrice <- threeMoPrice["1997-03::"]
threeMoPrice <- threeMoPrice[endpoints(threeMoPrice, "months"),]

offensive <- EAA(prices, cashAsset="VBMFX", bestN=3, returnWeights=TRUE)
defensive <- EAA(prices, cashAsset="VBMFX", bestN=3, wS=.5, wC=1)
offRF <- EAA(prices, cashAsset="VBMFX", bestN=3, monthlyRiskFree = threeMoPrice)
defRF <- EAA(prices, cashAsset="VBMFX", bestN=3, wS=.5, wC=1, monthlyRiskFree = threeMoPrice)
getSymbols("SPY", from="1998-01-01")
adSPY <- Ad(SPY)[index(defRF)]
SPYrets <- Return.calculate(adSPY)
compare <- cbind(offensive, defensive, offRF, defRF, SPYrets)
compare <- compare["1998-05::"]
colnames(compare) <- c("Offensive", "Defensive", "OffRF", "DefRF", "SPY")
stats <- rbind(Return.annualized(compare)*100, StdDev.annualized(compare)*100, maxDrawdown(compare)*100, SharpeRatio.annualized(compare))
rownames(stats)[3] <- "Worst Drawdown"
charts.PerformanceSummary(compare)
stats

diff <- compare[,1] - compare[,5]
zScore <- mean(diff)*sqrt(nrow(diff))/sd(diff)
