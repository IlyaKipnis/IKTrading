#'Flexible Asset Allocation returns algorithm
#'@description implements the Flexible Asset Allocation algorithm, by Keller and Van Putten (2012).
#'FAA takes the best assets as ranked by a weighted rank-sum of momentum, volatility, and correlation.
#'This implementation first filters on momentum above zero after performing all ranking and weight allocaations.
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
#'@param riskFreeInTie whether to allocate to the risk-free asset in the event of a tie (default TRUE)
#'@param returnWeights if TRUE, returns a length-two list of weights and returns (default FALSE)
#'@return a single xts of strategy returns
#'@references \url{http://quantstrattrader.wordpress.com/2014/10/31/combining-faa-and-stepwise-correlation/}
#'\cr \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735}
#'@export

FAA <- function(prices, monthLookback = 4,
                weightMom = 1, weightVol = .5, weightCor = .5, 
                riskFreeName = NULL, bestN = 3,
                stepCorRank = FALSE, stepStartMethod = c("best", "default"),
                geometric = TRUE, riskFreeInTie=TRUE, 
                returnWeights=FALSE, ...) {
  stepStartMethod <- stepStartMethod[1]
  if(is.null(riskFreeName)) {
    prices$zeroes <- 0
    riskFreeName <- "zeroes"
    warning("No risk-free security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
  
  prices <- na.locf(prices)
  riskFreeCol <- grep(riskFreeName, colnames(prices))
  if(is.na(prices[1,riskFreeCol])) {stop("Risk-free asset contains leading NAs. Please correct this.")}
  returns <- Return.calculate(prices)
  monthlyEps <- endpoints(prices, on = "months")
  tmp <- list()
  dates <- list()
  
  for(i in 2:(length(monthlyEps) - monthLookback)) {
    #subset data
    priceData <- prices[monthlyEps[i]:monthlyEps[i+monthLookback],]
    returnsData <- returns[monthlyEps[i]:monthlyEps[i+monthLookback],]
    
    #perform computations
    momentum <- data.frame(t(t(priceData[nrow(priceData),])/t(priceData[1,]) - 1))
    momentum[is.na(momentum)] <- -1
    momentum <- momentum[,!is.na(momentum)]
    #momentum[is.na(momentum)] <- -1 #set any NA momentum to negative 1 to keep R from crashing
    priceData <- priceData[,names(momentum)]
    returnsData <- returnsData[,names(momentum)]
    
    momRank <- rank(momentum)
    vols <- data.frame(StdDev(returnsData))
    volRank <- rank(-vols)
    cors <- cor(returnsData, use = "pairwise.complete")
    if (stepCorRank) {
      if(stepStartMethod=="best") {
        compositeMomVolRanks <- weightMom*momRank + weightVol*volRank
        maxRank <- compositeMomVolRanks[compositeMomVolRanks==max(compositeMomVolRanks)]
        corRank <- stepwiseCorRank(corMatrix=cors, startNames = names(maxRank), 
                                        bestHighestRank = TRUE, ...)
        
      } else {
        corRank <- stepwiseCorRank(corMatrix=cors, bestHighestRank=TRUE, ...)
      }
    } else {
      corRank <- rank(-rowSums(cors))
    }
    
    totalRank <- rank(weightMom*momRank + weightVol*volRank + weightCor*corRank)
    
    upper <- length(names(returnsData))
    lower <- max(upper-bestN+1, 1)
    topNvals <- sort(totalRank, partial=seq(from=upper, to=lower))[c(upper:lower)]
    
    #compute weights
    longs <- totalRank %in% topNvals #invest in ranks length - bestN or higher (in R, rank 1 is lowest)
    names(longs) <- names(totalRank)
    longs[momentum < 0] <- 0 #in previous algorithm, removed momentums < 0, this time, we zero them out at the end.
    if(!riskFreeInTie) { #remove cash from ties if specified
      if(riskFreeName %in% names(longs) & sum(longs) > bestN) {
        longs[riskFreeName] <- 0
      }
    }
    if(sum(longs)==0) {
      longs <- longs #if no assets selected, just keep as is--a bunch of zeroes
    } else {     
      longs <- longs/sum(longs) #equal weight all candidates
    }
    longs[longs > 1/bestN] <- 1/bestN #in the event that we have fewer than top N invested into, lower weights to 1/top N
    
    #append removed names (those with momentum NA)
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
  if(returnWeights) {
    return(list(weights, strategyReturns))
  }
  return(strategyReturns)
}