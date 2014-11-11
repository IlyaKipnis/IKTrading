require(IKTrading)
require(PerformanceAnalytics)
require(quantmod)

getSymbols("^GSPC", from="1800-01-01")
SPrets <- Return.calculate(Cl(GSPC))

goldFutures <- quandClean(stemCode = "CHRIS/CME_GC", verbose = TRUE)
getSymbols("GLD", from="1990-01-01") #quandl's data had a few gaps--let's use GLD to fill them in.
goldGLD <- merge(Cl(goldFutures), Cl(GLD), join='outer')
goldRets <- Return.calculate(goldGLD)
sum(is.na(goldRets[,1]))
mean(goldRets[is.na(goldRets[,1]),2], na.rm=TRUE)
goldRets[is.na(goldRets[,1]),1] <- goldRets[is.na(goldRets[,1]),2] #impute missing returns data with GLD returns data for that day
goldRets <- goldRets[,1]

thirtyBond <- quandClean(stemCode="CHRIS/CME_US", verbose = TRUE)
getSymbols("TLT", from="1990-01-01")
bondTLT <- merge(Cl(thirtyBond), Cl(TLT), join='outer')
bondRets <- Return.calculate(bondTLT)
sum(is.na(bondRets[,1]))
mean(bondRets[is.na(bondRets[,1]),2], na.rm=TRUE) #18 basis points? Just going to impute as zero.
bondRets[is.na(bondRets[,1]),1] <- 0 #there are 259 other such instances prior to this imputing
bondRets <- bondRets[,1]

SPbondGold <- cbind(SPrets, goldRets, bondRets)
SPbondGold <- SPbondGold["1977-12-30::"] #start off at beginning of 1978, since that's when gold futures were first in inception
colnames(SPbondGold) <- c("SandP", "Bonds", "Gold")

DTW_II_returns <- Return.rebalancing(R = SPbondGold, weights = c(1/3, 1/3, 1/3), geometric = FALSE, rebalance_on = "years")
stratSP <- merge(DTW_II_returns, SPrets, join='inner')
colnames(stratSP) <- c("Harry Long Article", "S&P 500")

#Recreate the original equity curve
charts.PerformanceSummary(stratSP["2004-12-01::"])

charts.PerformanceSummary(stratSP)

Return.cumulative(stratSP)
Return.annualized(stratSP)
SharpeRatio.annualized(stratSP)
maxDrawdown(stratSP)

stratSP$leveragedStrat <- stratSP[,1]*2
charts.PerformanceSummary(stratSP)

charts.PerformanceSummary(stratSP["1985::"])
maxDrawdown(stratSP["1985::"])
Return.annualized(stratSP["1985::"])
SharpeRatio.annualized(stratSP["1985::"])