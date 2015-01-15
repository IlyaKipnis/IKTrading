require(quantmod)
require(quantstrat)
require(IKTrading)

getSymbols("UNG", from="1990-01-01")
getSymbols("DGAZ", from="1990-01-01")
getSymbols("UGAZ", from="1990-01-01")
UNG <- UNG["2012-02-22::"]
UGAZ <- UGAZ["2012-02-22::"]

spread <- 3*OHLC(UNG) - OHLC(UGAZ)

nEMA=20

chart_Series(spread)
add_TA(EMA(Cl(spread), n=nEMA), on=1, col="blue", lwd=1.5)
legend(x=5, y=50, legend=c("EMA 20"),
       fill=c("blue"), bty="n")

signal <- Cl(spread) > EMA(Cl(spread), n=nEMA)
signal <- lag(signal)
spreadRets <- Return.calculate(Cl(spread))
stratRets <- signal*spreadRets
charts.PerformanceSummary(stratRets)
Return.annualized(stratRets)
maxDrawdown(stratRets)
SharpeRatio.annualized(stratRets)

signal <- Cl(spread) > EMA(Cl(spread), n=nEMA)
UNG$precomputedSig <- signal
UGAZ$precomputedSig <- signal*-1

initDate='1990-01-01'
currency('USD')
Sys.setenv(TZ="UTC")
symbols <- c("UNG", "UGAZ")
stock(symbols, currency="USD", multiplier=1)

strategy.st <- portfolio.st <- account.st <-"spread_strategy"

rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#long rules
add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputedSig", threshold=.5, 
                          relationship="gt", cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputedSig", threshold=.5, 
                          relationship="lt", cross=TRUE),
           label="longExit")

#short rules
add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputedSig", threshold=-.5, 
                          relationship="lt", cross=TRUE),
           label="shortEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputedSig", threshold=-.5, 
                          relationship="gt", cross=TRUE),
           label="shortExit")

#buy 3
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", orderqty=3), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#short 1
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="shortEntry", sigval=TRUE, ordertype="market", 
                        orderside="short", replace=FALSE, prefer="Open", orderqty=-1), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="shortExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="short", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

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


#portfolio cash PL
portString <- paste0("portfolio.", portfolio.st)
portPL <- .blotter[[portString]]$summary$Net.Trading.PL
portPL <- portPL[-1,]
plot(cumsum(portPL))

