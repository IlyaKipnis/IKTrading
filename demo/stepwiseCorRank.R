stepwiseCorRank <- function(corMatrix, startNames=NULL, stepSize=1, bestHighestRank=FALSE) {
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

adRets <- Return.calculate(adPrices)

subset <- adRets["2012"]
corMat <- cor(subset)

tmp <- list()
for(i in 1:length(mutualFunds)) {
  rankRow <- stepwiseCorRank(corMat, startNames=mutualFunds[i])
  tmp[[i]] <- rankRow
}
rankDemo <- do.call(rbind, tmp)
rownames(rankDemo) <- mutualFunds
origRank <- rank(rowSums(corMat))
rankDemo <- rbind(rankDemo, origRank)
rownames(rankDemo)[8] <- "Non-Sequential"

heatmap(-rankDemo, Rowv=NA, Colv=NA, col=heat.colors(8), margins=c(6,6))
