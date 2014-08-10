require(quantstrat)

initDate="1990-01-01"
from="2003-01-01"
to=as.character(Sys.Date())

currency('USD')
Sys.setenv(TZ="UTC")
symbols <- c("XLB")
getSymbols(symbols, from=from, to=to, src="yahoo", adjust=TRUE)
stock(symbols, currency="USD", multiplier=1)

strategy.st <- portfolio.st <- account.st <- "How_do_I_get_my_stoploss_on_an_indicator_working"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
SMA1=5
SMA2=10

#indicators
add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=SMA1),
              label="sma1")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=SMA2),
              label="sma2")

#signal
add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("SMA.sma1", "SMA.sma2"), relationship="gt"),
           label="longEntry")

#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty=100,
                        prefer="High",
                        pctATR=pctATR,
                        orderset="orders"), 
         type="enter", path.dep=TRUE, 
         label="entry") #need this label

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", 
                        sigval=TRUE, 
                        ordertype="stoplimit", 
                        orderside="long", 
                        replace=FALSE, 
                        orderqty='all',
                        order.price=quote(mktdata$SMA.sma2[timestamp]),
                        orderset="orders"), 
         type="chain", 
         parent="entry", #and this to link up the order
         label="stopLossLong",
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

#would like to exit when price hits orange line from above
chart.Posn(portfolio.st, "XLB")
add_TA(SMA(x=Cl(XLB), n=nSMA1), on=1, col="blue", lwd=1.5)
add_TA(SMA(x=Cl(XLB), n=nSMA2), on=1, col="orange", lwd=1.5)
zoom_Chart("2006")
