require(downloader)
download("https://dl.dropboxusercontent.com/s/jk6der1s5lxtcfy/XIVlong.TXT",
         destfile="longXIV.txt")
XIV <- read.csv("longXIV.txt", header=TRUE, stringsAsFactors=FALSE)
head(XIV)
XIV <- xts(XIV[,2:5], order.by=as.Date(XIV$Date))
XIVrets <- Return.calculate(Cl(XIV))
getSymbols("TLT", from="1990-01-01")
TLTrets <- Return.calculate(Cl(TLT))
adTltRets <- Return.calculate(Ad(TLT))
both <- merge(XIVrets, TLTrets, join='inner')
bothAd <- merge(XIVrets, adTltRets, join='inner')
stratTest <- Return.rebalancing(both, weights=c(.4, 1.8),
                                rebalance_on="weeks", geometric=FALSE)
adStratTest <- Return.rebalancing(bothAd, weights=c(.4, 1.8),
                                  rebalance_on="weeks", geometric=FALSE)
bothStrats <- merge(stratTest, adStratTest)
colnames(bothStrats) <- c("Close TLT", "Adjusted TLT")
getSymbols("SPY", from="1990-01-01")
ClSPY <- Return.calculate(Cl(SPY))
AdSPY <- Return.calculate(Ad(SPY))
SPYs <- cbind(ClSPY, AdSPY)
stratsAndBMs <- merge(bothStrats, SPYs, join='inner')
charts.PerformanceSummary(stratsAndBMs)
charts.PerformanceSummary(stratsAndBMs["2008::2009"])

Return.annualized(stratsAndBMs)
SharpeRatio.annualized(stratsAndBMs)
maxDrawdown(stratsAndBMs)

apply.yearly(stratsAndBMs, Return.cumulative)
apply.yearly(stratsAndBMs, maxDrawdown)
apply.yearly(stratsAndBMs, SharpeRatio.annualized)

applyWeeklySMA <- function(rets, n=200) {
  cumRets <- cumprod(1+rets)
  sma <- SMA(cumRets, n=n)
  smaCrosses <- xts(rep(NA, length(sma)), order.by=index(cumRets))
  smaCrosses[cumRets > sma & lag(cumRets) < sma] <- 1
  smaCrosses[cumRets < sma & lag(cumRets) > sma] <- 0
  smaCrosses <- na.locf(smaCrosses)
  weights <- xts(rep(NA, length(sma)), order.by=index(cumRets))
  weights[endpoints(sma, "weeks")] <- smaCrosses[endpoints(sma, "weeks")]
  weights <- lag(weights)
  weights <- na.locf(weights)
  weights[is.na(weights)] <- 1
  weightedRets <- rets*weights
  return(weightedRets)
}

tmp <- list()
for(i in seq(from=100, to=200, by=20)) {
  tmp[[i]] <- applyWeeklySMA(stratTest, n=i)
}
tmp <- do.call(cbind, tmp)
colnames(tmp) <- paste0("SMA_", seq(from=100, to=200, by=20))
origStratAsBM <- merge(tmp, stratTest)
colnames(origStratAsBM)[7] <- "No_SMA"
charts.PerformanceSummary(origStratAsBM, colorset=c("black", "blue", "red", "orange", "green", "purple", "darkgray"), 
                          main="SMAs and original strategy")

Return.annualized(origStratAsBM)
SharpeRatio.annualized(origStratAsBM)
maxDrawdown(origStratAsBM)

returnRisk <- data.frame(t(rbind(Return.annualized(origStratAsBM), maxDrawdown(origStratAsBM))))
chart.RiskReturnScatter(R=returnRisk, method="nocalc", add.sharpe=NA, main=NA)

data(edhec)
riskRet <- cbind(t(Return.annualized(edhec)), t(maxDrawdown(edhec)))
riskRet <- riskRet[1:4,]
chart.RiskReturnScatter(edhec, colorset=c("blue", "red", "black", "green"))
