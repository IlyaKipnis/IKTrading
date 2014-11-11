#'Stepwise Correlation Rank
#'@description computes a stepwise correlation ranking of vectors starting from a given subset of vectors
#'@param corMatrix a correlation matrix
#'@param startNames names of the vectors to start initial ranking from. If NULL, the stepSize amount of vectors with the lowest
#'overall correlation will be the initial vectors to start the process. Default NULL
#'@param stepSize how many vectors per ranking step (EG stepSize of 2 adds the 2 lowest correlation-ranked vectors to 
#'existing set at each step). Default 1
#'@param bestHighestRank whether or not to assign the highest rank (number of vectors) to the minimum-correlated vector(s). 
#'TRUE variant used in Flexible Asset Allocation (FAA). Default FALSE
#'@return a set of ranks for the given names of the vectors in the correlation matrix
#'@references \url{http://quantstrattrader.wordpress.com/2014/10/27/introducing-stepwise-correlation-rank/}
#'\cr \url{http://cssanalytics.wordpress.com/2014/10/27/flexible-asset-allocation-with-conditional-correlations/}
#'@export
"stepwiseCorRank" <- function(corMatrix, startNames=NULL, stepSize=1, bestHighestRank=FALSE) {
  #edge cases
  if(dim(corMatrix)[1] == 1) {
    return(corMatrix)
  } else if (dim(corMatrix)[1] == 2) {
    ranks <- c(1.5, 1.5)
    names(ranks) <- colnames(corMatrix)
    return(ranks)
  }
  
  if(is.null(startNames)) {
    corSums <- rowSums(corMatrix)
    corRanks <- rank(corSums)
    startNames <- names(corRanks)[corRanks <= stepSize]
  }
  nameList <- list()
  nameList[[1]] <- startNames
  rankList <- list()
  rankCount <- 1
  rankList[[1]] <- rep(rankCount, length(startNames))
  rankedNames <- do.call(c, nameList)
  
  while(length(rankedNames) < nrow(corMatrix)) {
    rankCount <- rankCount+1
    subsetCor <- corMatrix[, rankedNames]
    if(class(subsetCor) != "numeric") {
      subsetCor <- subsetCor[!rownames(corMatrix) %in% rankedNames,]
      if(class(subsetCor) != "numeric") {
        corSums <- rowSums(subsetCor)
        corSumRank <- rank(corSums)
        lowestCorNames <- names(corSumRank)[corSumRank <= stepSize]
        nameList[[rankCount]] <- lowestCorNames
        rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
      } else { #1 name remaining
        nameList[[rankCount]] <- rownames(corMatrix)[!rownames(corMatrix) %in% names(subsetCor)]
        rankList[[rankCount]] <- rankCount
      }
    } else {  #first iteration, subset on first name
      subsetCorRank <- rank(subsetCor)
      lowestCorNames <- names(subsetCorRank)[subsetCorRank <= stepSize]
      nameList[[rankCount]] <- lowestCorNames
      rankList[[rankCount]] <- rep(rankCount, min(stepSize, length(lowestCorNames)))
    }    
    rankedNames <- do.call(c, nameList)
  }
  
  ranks <- do.call(c, rankList)
  names(ranks) <- rankedNames
  if(bestHighestRank) {
    ranks <- 1+length(ranks)-ranks
  }
  ranks <- ranks[colnames(corMatrix)] #return to original order
  return(ranks)
}