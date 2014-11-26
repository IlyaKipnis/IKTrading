#'Flexible Asset Allocation returns algorithm
#'@description implements a modification of the Flexible Asset Allocation algorithm, by Keller and Van Putten (2012).
#'FAA takes the best assets as ranked by a weighted rank-sum of momentum, volatility, and correlation.
#'This implementation first filters on momentum above zero before proceeding with ranking.
#'@param prices a price history for assets intended to be traded over the course of the simulation. Need not be contemporaneous.
#'@param monthLookback a monthly lookback period over which to compute momentum, volatility, and correlations. (Default 4)
#'@param weightMom the weight to put on the momentum rank in the calculation 
#'weightMom*rankMom + weightVol*rankVol + weightCor*rankCor (default 1)
#'@param weightVol the weight of the volatility rank in the above equation (default .5)
#'@param weightCor the weight of the correlation rank in the aforementioned equation (default .5)
#'@param riskFreeName name of the security to be used as the risk free rate. If NULL, will use a vector of zeroes. (Default NULL) 
#'Recommended to use quandClean('CHRIS/CME_US'), SHY, or VFISX
#'@param bestN the top N securities to invest in. Will invest in more than bestN in the event of a tie for the lowest bestN.
#'@param stepCorRank whether or not to use the stepwise correlation rank (see stepwiseCorRank function). If FALSE, ranks 
#'correlations based on sums of correlations of each asset to all other assets. (Default FALSE)
#'@param stepStartMethod the method to determine the starting asset for the stepwise correlation rank algorithm, if enabled. 
#'The 'best' argument chooses the best ranked asset by the momentum and volatility weights, while the 'default' method starts with
#'the initial lowest-correlated asset. (Default 'best')
#'@param geometric whether or not to use geometric compounding for returns (default TRUE)
#'@return a single xts of strategy returns
#'@references \url{http://quantstrattrader.wordpress.com/2014/10/31/combining-faa-and-stepwise-correlation/}
#'\cr \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735}
#'@export
"FAAreturns" <- function(prices, monthLookback = 4,
                       weightMom = 1, weightVol = .5, weightCor = .5, 
                       riskFreeName = NULL, bestN = 3,
                       stepCorRank = FALSE, stepStartMethod = c("best", "default"),
                       geometric = TRUE) {
  stepStartMethod <- stepStartMethod[1]
  if(is.null(riskFreeName)) {
    prices$zeroes <- 0
    riskFreeName <- "zeroes"
    warning("No risk-free security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
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
    momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[, momentum > 0] #remove securities with momentum < 0
    returnsData <- returnsData[, momentum > 0]
    momentum <- momentum[momentum > 0]
    names(momentum) <- colnames(returnsData)
    
    if(length(momentum) > 1) {
      vol <- as.numeric(-sd.annualized(returnsData)) #only need to compute volatility if there's more than 1 security
      #perform ranking
      if(!stepCorRank) {
        sumCors <- -colSums(cor(returnsData, use="complete.obs"))
        stats <- data.frame(cbind(momentum, vol, sumCors))
        ranks <- data.frame(apply(stats, 2, rank))
        weightRankSum <- weightMom*ranks$momentum + weightVol*ranks$vol + weightCor*ranks$sumCors
        names(weightRankSum) <- rownames(ranks)
      } else {
        corMatrix <- cor(returnsData, use="complete.obs")
        momRank <- rank(momentum)
        volRank <- rank(vol)
        compositeMomVolRanks <- weightMom*momRank + weightVol*volRank
        maxRank <- compositeMomVolRanks[compositeMomVolRanks==max(compositeMomVolRanks)]
        if(stepStartMethod=="default") {
          stepCorRanks <- stepwiseCorRank(corMatrix=corMatrix, startNames = NULL, 
                                          stepSize = 1, bestHighestRank = TRUE)
        } else {
          stepCorRanks <- stepwiseCorRank(corMatrix=corMatrix, startNames = names(maxRank), 
                                          stepSize = 1, bestHighestRank = TRUE)
        }
        weightRankSum <- weightMom*momRank + weightVol*volRank + weightCor*stepCorRanks
      }
      
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
      names(longs) <- names(totalRank)
      
    } else if(length(momentum) == 1) { #only one security had positive momentum 
      longs <- 1/bestN
      names(longs) <- names(momentum)
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
    dates[[i]] <- as.character(index(returnsData)[nrow(returnsData)])
  }
  
  weights <- do.call(rbind, tmp)
  dates <- do.call(c, dates)
  weights <- xts(weights, order.by=as.Date(dates)) 
  weights[, riskFreeCol] <- weights[, riskFreeCol] + 1-rowSums(weights)
  strategyReturns <- Return.rebalancing(R = returns, weights = weights, geometric = geometric)
  colnames(strategyReturns) <- paste(monthLookback, weightMom, weightVol, weightCor, sep="_")
  return(strategyReturns)
}
