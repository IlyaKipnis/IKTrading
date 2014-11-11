require(PerformanceAnalytics)
require(quantmod)

mutualFunds <- c("VTSMX", #Vanguard Total Stock Market Index
                 "FDIVX", #Fidelity Diversified International Fund
                 "VEIEX", #Vanguard Emerging Markets Stock Index Fund
                 "VFISX", #Vanguard Short-Term Treasury Fund
                 "VBMFX", #Vanguard Total Bond Market Index Fund
                 "QRAAX", #Oppenheimer Commodity Strategy Total Return 
                 "VGSIX" #Vanguard REIT Index Fund
)

#mid 1997 to end of 2012
getSymbols(mutualFunds, from="1997-06-30", to="2012-12-31")
tmp <- list()
for(fund in mutualFunds) {
  tmp[[fund]] <- Ad(get(fund))
}

#always use a list hwne intending to cbind/rbind large quantities of objects
adPrices <- do.call(cbind, args = tmp)
colnames(adPrices) <- gsub(".Adjusted", "", colnames(adPrices))

FAAreturns <- function(prices, monthLookback = 4,
                       weightMom=1, weightVol=.5, weightCor=.5, 
                       riskFreeName="VFISX", bestN=3) {
  
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthLookback)) {
    
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    priceData <- priceData[, momentum > 0] #remove securities with momentum < 0
    returnsData <- returnsData[, momentum > 0]
    momentum <- momentum[momentum > 0]
    names(momentum) <- colnames(returnsData)
    
    vol <- as.numeric(-sd.annualized(returnsData))
    #sumCors <- -colSums(cor(priceData[endpoints(priceData, on="months")]))
    sumCors <- -colSums(cor(returnsData, use="complete.obs"))
    stats <- data.frame(cbind(momentum, vol, sumCors))
    
    if(nrow(stats) > 1) {
      
      #perform ranking
      ranks <- data.frame(apply(stats, 2, rank))
      weightRankSum <- weightMom*ranks$momentum + weightVol*ranks$vol + weightCor*ranks$sumCors
      totalRank <- rank(weightRankSum)
      
      #find top N values, from http://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column
      #thanks to Dr. Rob J. Hyndman
      upper <- length(names(returnsData))
      lower <- max(upper-bestN+1, 1)
      topNvals <- sort(totalRank, partial=seq(from=upper, to=lower))[c(upper:lower)]
      
      #compute weights
      longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
      longs <- longs/sum(longs) #equal weight all candidates
      longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
      names(longs) <- rownames(ranks)
      
    } else if(nrow(stats) == 1) { #only one security had positive momentum 
      longs <- 1/bestN
      names(longs) <- rownames(stats)
    } else { #no securities had positive momentum 
      longs <- 1
      names(longs) <- riskFreeName
    }
    
    #append removed names (those with momentum < 0)
    removedZeroes <- rep(0, ncol(returns)-length(longs))
    names(removedZeroes) <- names(returns)[!names(returns) %in% names(longs)]
    longs <- c(longs, removedZeroes)
    
    #reorder to be in the same column order as original returns/prices
    longs <- data.frame(t(longs))
    longs <- longs[, names(returns)]
    
    #append lists
    tmp[[i]] <- longs
    dates[[i]] <- index(returnsData)[nrow(returnsData)]
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = FALSE)
  colnames(strategyReturns) <- paste(monthLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}

weightMom <- seq(0, 1, by=.5)
weightVol <- c(0, .5, 1)
weightCor <- c(0, .5, 1)
monthLookback=c(3, 4, 6, 10)
permutations <- expand.grid(weightMom, weightVol, weightCor, monthLookback)
colnames(permutations) <- c("wMom", "wVol", "wCor", "monthLookback")

require(doMC)
registerDoMC(detectCores())
t1 <- Sys.time()
out <- foreach(i = 1:nrow(permutations), .combine = cbind) %dopar% {
  FAAreturns(prices=adPrices, 
             monthLookback = permutations$monthLookback[i], 
             weightMom = permutations$wMom[i], 
             weightCor = permutations$wCor[i], 
             weightVol=permutations$wVol[i])
}
t2 <- Sys.time()
print(t2-t1)

out <- out["1998-10::"] #start at 1999 due to NAs with

FAAwalkForward <- function(portfolios, applySubset = apply.quarterly, applyFUN = Return.cumulative) {
  metrics <- applySubset(portfolios, applyFUN)
  weights <- list()
  for(i in 1:nrow(metrics)) {
    row <- metrics[i,]
    winners <- row==max(row)
    weight <- winners/rowSums(winners) #equal weight all top performers
    weights[[i]] <- weight
  }
  weights <- do.call(rbind, weights)
  returns <- Return.rebalancing(portfolios, weights)
  return(returns)
}

WFQtrRets <- FAAwalkForward(portfolios = out, applySubset = apply.quarterly, applyFUN = Return.cumulative)
WFYrRets <- FAAwalkForward(portfolios = out, applySubset = apply.yearly, applyFUN = Return.cumulative)
WFMoRets <- FAAwalkForward(portfolios = out, applySubset = apply.monthly, applyFUN = Return.cumulative)

WF <- cbind(WFQtrRets, WFYrRets, WFMoRets)
colnames(WF) <- c("quarterly", "annually", "monthly")
WF <- WF["1999::"]

original <- FAAreturns(adPrices)
original <- original["1999::"]
WF <- cbind(WF, original)
colnames(WF)[4] <- "original"
charts.PerformanceSummary(WF)

Return.annualized(WF)
maxDrawdown(WF)
SharpeRatio.annualized(WF)

