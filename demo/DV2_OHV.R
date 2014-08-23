require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2003-01-01"
to=as.character(Sys.Date())
options(width=70)

source("demoData.R")

#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "DV2_OHV"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
nAvg=2
pctLookback=126
maType="SMA"
deTrend=FALSE
nDT=126

n1=3
n2=10

entryThresh=30
exitThresh=70
volThresh=1

period=10
pctATR=.02

#indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")

add.indicator(strategy.st, name="DVO",
              arguments=list(HLC=quote(HLC(mktdata)), nAvg=nAvg, 
                             pctLookback=pctLookback, maType=maType, 
                             deTrend=deTrend, nDT=nDT),
              label="dv2")

add.indicator(strategy.st, name="OHV",
              arguments=list(x=quote(Cl(mktdata)), n1=n1, n2=n2),
              label="ohv")
#signals
add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="DVO.dv2", threshold=entryThresh,
                          relationship="lt", cross=FALSE),
           label="dv2LtThresh")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="OHV.ohv", threshold=volThresh,
                          relationship="lt", cross=FALSE),
           label="ohvLtThresh")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("dv2LtThresh", "ohvLtThresh"), cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="DVO.dv2", threshold=exitThresh,
                          relationship="gt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="OHV.ohv", threshold=volThresh,
                          relationship="gt", cross=TRUE),
           label="volExit")

#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="volExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

#set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)


#trade statistics
tStats <- tradeStats(Portfolios = portfolio.st, use="trades", inclZeroDays=FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
print(data.frame(t(tStats[,-c(1,2)])))
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Percent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE))

#daily and duration statistics
dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
print(data.frame(t(dStats)))
durStats <- durationStatistics(Portfolio=portfolio.st, Symbols=sort(symbols))
indivDurStats <- durationStatistics(Portfolio=portfolio.st, Symbols=sort(symbols), aggregate=FALSE)
print(t(durStats))
print(t(indivDurStats))

#market exposure
tmp <- list()
length(tmp) <- length(symbols)
for(i in 1:nrow(dStats)) {
  totalDays <- nrow(get(rownames(dStats)[i]))
  mktExposure <- dStats$Total.Days[i]/totalDays
  tmp[[i]] <- c(rownames(dStats)[i], round(mktExposure, 3))
}
mktExposure <- data.frame(do.call(rbind, tmp))
colnames(mktExposure) <- c("Symbol","MktExposure")
print(mktExposure)
print(mean(as.numeric(as.character(mktExposure$MktExposure))))

#portfolio cash PL
portString <- paste0("portfolio.", portfolio.st)
portPL <- .blotter[[portString]]$summary$Net.Trading.PL

#Cash Sharpe
(SharpeRatio.annualized(portPL, geometric=FALSE))

#Portfolio comparisons to SPY
instRets <- PortfReturns(account.st)

#Correlations
instCors <- cor(instRets)
diag(instRets) <- NA
corMeans <- rowMeans(instCors, na.rm=TRUE)
names(corMeans) <- gsub(".DailyEndEq", "", names(corMeans))
print(round(corMeans,3))
mean(corMeans)

portfRets <- xts(rowMeans(instRets)*ncol(instRets), order.by=index(instRets))
portfRets <- portfRets[!is.na(portfRets)]
cumPortfRets <- cumprod(1+portfRets)
firstNonZeroDay <- as.character(index(portfRets)[min(which(portfRets!=0))])
getSymbols("SPY", from=firstNonZeroDay, to=to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1+SPYrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison)  <- c("strategy", "SPY")
chart.TimeSeries(comparison, legend.loc = "topleft",
                 colors=c("green","red"))
chart.RelativePerformance(portfRets,SPYrets)

SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

dailyRetComparison <- cbind(portfRets, SPYrets)
colnames(dailyRetComparison)  <- c("strategy", "SPY")
round(apply.yearly(dailyRetComparison, Return.cumulative),3)
round(apply.yearly(dailyRetComparison, SharpeRatio.annualized),3)
round(apply.yearly(dailyRetComparison, maxDrawdown),3)

chart.Posn(portfolio.st, "XLB")
tmp <- DVO(HLC=HLC(XLB), nAvg=nAvg, pctLookback=pctLookback, maType=maType, deTrend=deTrend, nDT=nDT)
add_TA(tmp, col="purple")
tmp <- tmp - tmp + entryThresh
add_TA(tmp, on=5, col="blue", lwd=1.5)
tmp <- tmp - tmp + exitThresh
add_TA(tmp, on=5, col="red", lwd=1.5)
tmp <- OHV(x=Cl(XLB), n1=n1, n2=n2)
add_TA(tmp, col="purple")
tmp <- tmp-tmp+volThresh
add_TA(tmp, on=6, col="blue", lwd=1.5)
