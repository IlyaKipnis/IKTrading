require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2003-01-01"
to="2012-12-31"
options(width=70)

source("demoData.R")

#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "DollarVsATRos"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
pctATR <- .02
period <- 10
atrOrder <- TRUE

nRSI <- 2
buyThresh <- 20
sellThresh <- 80
nSMA <- 200

#indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")

add.indicator(strategy.st, name="RSI",
              arguments=list(price=quote(Cl(mktdata)), n=nRSI),
              label="rsi")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=nSMA),
              label="sma")

#signals
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("Close", "sma"), relationship="gt"),
           label="filter")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="rsi", threshold=buyThresh, 
                          relationship="lt", cross=FALSE),
           label="rsiLtThresh")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("filter", "rsiLtThresh"), cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="rsi", threshold=sellThresh,
                          relationship="gt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "sma"), relationship="lt"),
           label="filterExit")

#rules
if(atrOrder) {
  
  add.rule(strategy.st, name="ruleSignal", 
           arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                          orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                          tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
           type="enter", path.dep=TRUE)
} else { 
  add.rule(strategy.st, name="ruleSignal", 
           arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                          orderside="long", replace=FALSE, prefer="Open", osFUN=osMaxDollar,
                          tradeSize=tradeSize, maxSize=tradeSize), 
           type="enter", path.dep=TRUE)
}


add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="filterExit", sigval=TRUE, orderqty="all", ordertype="market", 
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


#Portfolio comparisons to SPY
instRets <- PortfReturns(account.st)

if(atrOrder) {  
  atrSd <- StdDev(na.omit(instRets), portfolio_method="component")
  t1  <- Sys.time()
  atrES <- ES(na.omit(instRets), portfolio_method="component")
  t2 <- Sys.time()
  print(t2-t1)
} else {
  dollarSd <- StdDev(na.omit(instRets), portfolio_method="component")
  t1  <- Sys.time()
  dollarES <- ES(na.omit(instRets), portfolio_method="component")
  t2 <- Sys.time()
  print(t2-t1)
}

if(("atrSd" %in% ls()) & ("dollarSd" %in% ls())){  
  boxPlotFrame <- cbind(atrSd$pct_contrib_StdDev, 
                        atrES$pct_contrib_MES,
                        dollarSd$pct_contrib_StdDev,
                        dollarES$pct_contrib_MES)
  colnames(boxPlotFrame) <- c("atrSd", "atrES", "dollarSd", "dollarES")
  boxplot(boxPlotFrame)
}

rownames(boxPlotFrame) <- gsub(".DailyEndEq", "", rownames(boxPlotFrame))
