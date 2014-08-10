hammer <- function(OHLC, profMargin=1.5) {
  dailyMax <- pmax(Op(OHLC), Cl(OHLC))
  dailyMin <- pmin(Op(OHLC), Cl(OHLC))
  upShadow <- Hi(OHLC) - dailyMax
  dnShadow <- dailyMin - Lo(OHLC)
  body <- dailyMax-dailyMin
  hammerDay <- dnShadow/body > 2 & dnShadow/upShadow > 5
  hammers <- OHLC[hammerDay==1,]
  hammers$stopLoss <- 4/3*Lo(hammers)-1/3*Hi(hammers)
  hammers$takeProfit <- Hi(hammers) + (Hi(hammers)-hammers$stopLoss)*profMargin
  hammers <- cbind(hammerDay, hammers$stopLoss, hammers$takeProfit)
  hammers$stopLoss <- na.locf(hammers$stopLoss)
  hammers$takeProfit <- na.locf(hammers$takeProfit)
  colnames(hammers) <- c("hammer", "SL", "TP")
  return(hammers)
}

require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2003-01-01"
to=as.character(Sys.Date())
options(width=70)
verbose=TRUE

source("demoData.R")

#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "Hammer_4TP"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
nSMA1=10
nSMA2=30
nSMA3=5
profMargin=1.5

period=10
pctATR=.1


#indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), 
                             n=period), 
              label="atrX")

add.indicator(strategy.st, name="hammer",
              arguments=list(OHLC=quote(OHLC(mktdata)), 
                             profMargin=profMargin),
              label="hammer")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA1),
              label="sma1")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA2),
              label="sma2")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), 
                             n=nSMA3),
              label="sma3")
#signals
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("SMA.sma1", "SMA.sma2"), 
                          relationship="gt"),
           label="upTrend")

add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("SMA.sma3", "SMA.sma1"), 
                          relationship="lt"),
           label="pullback")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="hammer.hammer", threshold=.5, 
                          relationship="gt", cross=TRUE),
           label="hammerDay")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("upTrend", 
                                    "pullback", 
                                    "hammerDay"), 
                          cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("SMA.sma1", "SMA.sma2"), 
                          relationship="lt"),
           label="SMAexit")
#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        osFUN=osDollarATR,
                        tradeSize=tradeSize, 
                        prefer="High",
                        pctATR=pctATR,
                        atrMod="X",
                        orderset="orders"), 
         type="enter", path.dep=TRUE,
         label="hammerEntry")
 
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty='all',
                        order.price=quote(mktdata$SL.hammer[timestamp]),
                        orderset="orders"), 
         type="chain", 
         parent="hammerEntry",
         label="stopLossLong",
         path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="limit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty='all',
                        order.price=quote(mktdata$TP.hammer[timestamp]),
                        orderset="orders"), 
         type="chain", 
         parent="hammerEntry",
         label="takeProfitLong",
         path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal",
         arguments=list(sigcol="SMAexit",
                        sigval=TRUE,
                        ordertype="market",
                        orderside="long",
                        replace=FALSE,
                        orderqty='all',
                        prefer='Open',
                        orderset='orders'
                        ),
         type='exit',
         label='SMAexitLong',
         path.dep=TRUE)

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
#dStats <- dailyStats(Portfolios = portfolio.st, use="Equity")
#rownames(dStats) <- gsub(".DailyEndEq","", rownames(dStats))
#print(data.frame(t(dStats)))
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

chart.Posn(portfolio.st, "EFA")
add_TA(SMA(Cl(EFA), n=nSMA1), on=1, lwd=1.5, col="blue")
add_TA(SMA(Cl(EFA), n=nSMA2), on=1, lwd=1.5, col="purple")
add_TA(SMA(Cl(EFA), n=nSMA3), on=1, lwd=1.5, col="orange")
tmp <- hammer(OHLC=OHLC(EFA), profMargin=profMargin)
add_TA(tmp$hammer)
add_TA(tmp$SL, on=1, col="red", lwd=1.5)
add_TA(tmp$TP, on=1, col="green", lwd=1.5)

