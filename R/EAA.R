#'Elastic Asset Allocation (EAA)
#'@description Asset allocation algorithm from Keller (2014-12-30). EAA performs asset selection and weighting
#'based on weights for returns, volatility, and correlation to the equally-weighted universe.
#'@param monthlyPrices a price series using monthly data
#'@param wR the weight to place on returns (default 1)
#'@param wV the weight for volatility (default 0)
#'@param wC the weight for correlations (default .5)
#'@param wS a selection aggressiveness weight. A weight of infinity will result in a single security. 
#'A weight of 0 will result in near equal weights of the selected securities. (default 2)
#'@param errorJitter a small error term due to volatility in the denomiantor (default 1e-6)
#'@param cashAsset the name (string) of the asset to be used as the cash asset. If NULL, will result in zeroes. (default NULL)
#'@param bestN the number of securities to select every period (default 1 + square root of number of assets in universe)
#'@param enableCrashProtection if enabled, will result in weights being multiplied by 1-n/(size of universe), 
#'where n is the number of securities with negative absolute returns (default TRUE)
#'@param returnWeights whether or not to return the portfolio weights along with the returns in a list format (default FALSE)
#'@param monthlyRiskFree if provided, a time series of prices of a risk-free asset (E.G.: IRX),
#' for use in subtracting from asset returns to compute asset returns (default NULL)
#'@return a monthly xts of the strategy's returns
#'@references \url{https://quantstrattrader.wordpress.com/2015/01/03/for-a-new-year-a-new-asset-allocation-system-just-published-in-ssrn/}
#'\cr \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979}
#'@export
"EAA" <- function(monthlyPrices, wR=1, wV=0, wC=.5, wS=2, errorJitter=1e-6, 
                cashAsset=NULL, bestN=1+ceiling(sqrt(ncol(monthlyPrices))),
                enableCrashProtection = TRUE, returnWeights=FALSE, monthlyRiskFree=NULL) {
  returns <- Return.calculate(monthlyPrices)
  returns <- returns[-1,] #return calculation uses one observation
  if(!is.null(monthlyRiskFree)) {
    returnsRF <- Return.calculate(monthlyRiskFree)
    returnsRF <- returnsRF[-1,]
  }
  
  if(is.null(cashAsset)) {
    returns$zeroes <- 0
    cashAsset <- "zeroes"
    warning("No cash security specified. Recommended to use one of: quandClean('CHRIS/CME_US'), SHY, or VFISX. 
            Using vector of zeroes instead.")
  }
  
  cashCol <- grep(cashAsset, colnames(returns))
  
  weights <- list()
  for(i in 1:(nrow(returns)-11)) {
    returnsData <- returns[i:(i+11),] #each chunk will be 12 months of returns data
    #per-month mean of cumulative returns of 1, 3, 6, and 12 month periods
    periodReturn <- ((returnsData[12,] + Return.cumulative(returnsData[10:12,]) + 
                      Return.cumulative(returnsData[7:12,]) + Return.cumulative(returnsData)))/22
    
    if(!is.null(monthlyRiskFree)) {
      rfData <- returnsRF[i:(i+11),]
      rfReturn <- ((rfData[12,] + Return.cumulative(rfData[10:12,]) + 
                    Return.cumulative(rfData[7:12,]) + Return.cumulative(rfData)))/22
      periodReturn <- periodReturn - as.numeric(rfReturn)
    }
    
    vols <- StdDev.annualized(returnsData) 
    mktIndex <- xts(rowMeans(returnsData, na.rm=TRUE), order.by=index(returnsData)) #equal weight returns of universe
    cors <- cor(returnsData, mktIndex) #correlations to market index
    
    weightedRets <- periodReturn ^ wR
    weightedCors <- (1 - as.numeric(cors)) ^ wC
    weightedVols <- (vols + errorJitter) ^ wV
    wS <- wS + errorJitter
    
    z <- (weightedRets * weightedCors / weightedVols) ^ wS #compute z_i and zero out negative returns
    z[periodReturn < 0] <- 0
    crashProtection <- sum(z==0, na.rm=TRUE)/sum(!is.na(z)) #compute crash protection cash cushion
    
    orderedZ <- sort(as.numeric(z), decreasing=TRUE)
    selectedSecurities <- z >= orderedZ[bestN]
    preNormalizedWeights <- z*selectedSecurities #select top N securities, keeping z_i scores
    periodWeights <- preNormalizedWeights/sum(preNormalizedWeights, na.rm=TRUE) #normalize
    if (enableCrashProtection) {
      periodWeights <- periodWeights * (1-crashProtection) #CP rule
    }
    periodWeights[is.na(periodWeights)] <- 0
    weights[[i]] <- periodWeights
  }
  
  weights <- do.call(rbind, weights)
  weights[, cashCol] <- weights[, cashCol] + 1-rowSums(weights) #add to risk-free asset all non-invested weight
  strategyReturns <- Return.rebalancing(R = returns, weights = weights) #compute strategy returns
  if(returnWeights) {
    return(list(weights, strategyReturns))
  } else {
    return(strategyReturns)
  }
}