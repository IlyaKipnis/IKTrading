getSymbols("VWEHX", from="1950-01-01")
getSymbols("SPY", from="1900-01-01")
getSymbols("HYG", from="1990-01-01")
getSymbols("SHY", from="1990-01-01")
getSymbols("VFISX", from="1990-01-01")

spySma20Cl <- SMA(Cl(SPY), n=20)
clSig <- Cl(SPY) > spySma20Cl
clSig <- lag(clSig, 1)

vwehxCloseRets <- Return.calculate(Cl(VWEHX))
vfisxCloseRets <- Return.calculate(Cl(VFISX))
vwehxAdjustRets <- Return.calculate(Ad(VWEHX))
vfisxAdjustRets <- Return.calculate(Ad(VFISX))

hygCloseRets <- Return.calculate(Cl(HYG))
shyCloseRets <- Return.calculate(Cl(SHY))
hygAdjustRets <- Return.calculate(Ad(HYG))
shyAdjustRets <- Return.calculate(Ad(SHY))

mutualAdRets <- vwehxAdjustRets*clSig + vfisxAdjustRets*(1-clSig)
mutualClRets <- vwehxCloseRets*clSig + vfisxCloseRets*(1-clSig)

etfAdRets <- hygAdjustRets*clSig + shyAdjustRets*(1-clSig)
etfClRets <- hygCloseRets*clSig + shyCloseRets*(1-clSig)

mutualFundBacktest <- merge(mutualAdRets, mutualClRets, join='inner')
charts.PerformanceSummary(mutualFundBacktest)
data.frame(t(rbind(Return.annualized(mutualFundBacktest)*100, 
                 maxDrawdown(mutualFundBacktest)*100,
                 SharpeRatio.annualized(mutualFundBacktest))))

etfBacktest <- merge(etfAdRets, etfClRets, join='inner')
charts.PerformanceSummary(etfBacktest)
data.frame(t(rbind(Return.annualized(etfBacktest)*100, 
                   maxDrawdown(etfBacktest)*100,
                   SharpeRatio.annualized(etfBacktest))))

fundsAndETFs <- merge(mutualFundBacktest, etfBacktest, join='inner')
charts.PerformanceSummary(fundsAndETFs)
data.frame(t(rbind(Return.annualized(fundsAndETFs)*100, 
                   maxDrawdown(fundsAndETFs)*100,
                   SharpeRatio.annualized(fundsAndETFs))))

require(quantstrat)

currency('USD')
Sys.setenv(TZ="UTC")
symbols <- "HYG"
stock(symbols, currency="USD", multiplier=1)
initDate="1990-01-01"

strategy.st <- portfolio.st <- account.st <- "preCalc"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

clSig <- Cl(SPY) > SMA(Cl(SPY), n=20)

HYG <- merge(HYG, clSig, join='inner')
names(HYG)[7] <- "precomputed_signal"

#no parameters--we precalculated our signal

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputed_signal", threshold=.5, 
                          relationship="gt", cross=TRUE),
           label="longEntry")


add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputed_signal", threshold=.5, 
                          relationship="lt", cross=TRUE),
           label="longExit")

add.rule(strategy.st, name="ruleSignal",
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=1, ordertype="market",
                        orderside="long", replace=FALSE, prefer="Open"),
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
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

data.frame(round(t(tradeStats(portfolio.st)[-c(1,2)]),2))

chart.Posn(portfolio.st, symbol="HYG")
