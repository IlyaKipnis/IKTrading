#'Quandl Futures Data Cleaning
#'@description Constructs a continuous futures time series from front and back month contracts
#'from Quandl's database.
#'@param stemCode the characters for the corresponding futures series. Usually in the form of
#'CHRIS/EXCHANGE_SYMBOL, such as CHRIS/CME_CL for light, sweet crude oil, and CHRIS/CME_GC for gold.
#'@param start_date a character string date in the form of yyyy-mm-dd such as 2000-01-01
#'@param end_date a character string date identical to the above
#'@param verbose an argument that displays how many NA and spike days were removed, then prints the removed dates
#'@return an OHLCVI time series of daily data
#'@export
"quandClean" <- function(stemCode, start_date=NULL, end_date=NULL, verbose=FALSE, ...) {
  require(Quandl)
  if(is.null(start_date)) {start_date <- Sys.Date()-365*1000}
  if(is.null(end_date)) {end_date <- Sys.Date()+365*1000}
  frontCode <- paste0(stemCode, 1)
  backCode <- paste0(stemCode, 2)
  front <- Quandl(frontCode, type="xts", start_date=start_date, end_date=end_date, ...) 
  interestColname <- colnames(front)[grep(pattern="Interest", colnames(front))]
  volColname <- colnames(front)[grep(pattern="Volume", colnames(front))]
  if("Close" %in% colnames(front)) {
    front <- front[,c("Open","High","Low","Close",volColname,interestColname)]
  } else {
    front <- front[,c("Open","High","Low","Settle",volColname,interestColname)]
  }
  colnames(front) <- c("O","H","L","C","V","OI")
  back <- Quandl(backCode, type="xts", start_date=start_date, end_date=end_date, ...)
  if("Close" %in% colnames(back)) {    
    back <- back[,c("Open","High","Low","Close",volColname,interestColname)]
  } else {
    back <- back[,c("Open","High","Low","Settle",volColname,interestColname)]
  }
  colnames(back)  <- c("BO","BH","BL","BS","BV","BI") #B for Back
  
  #combine front and back for comparison
  both <- cbind(front,back)
  
  #impute NAs in open interest with -1
  both$BI[is.na(both$BI)] <- -1
  both$OI[is.na(both$OI)] <- -1
  both$lagBI <- lag(both$BI)
  both$lagOI <- lag(both$OI)
  
  #impute bad back month open-interest prints -- if it is truly a low quantity, it won't make a difference in the computation.
  both$OI[both$OI==-1] <- both$lagOI[both$OI==-1]
  both$BI[both$BI==-1] <- both$lagBI[both$BI==-1]
  both$OIdiff <- both$OI - both$BI
  both$tracker <- NA
  
  #the formal open interest cross from front to back
  both$tracker[both$OIdiff < 0] <- -1
  both$tracker <- lag(both$tracker) #since we have to observe OI cross, we roll next day
  
  #any time we're not on the back contract, we're on the front contract
  both$tracker[both$OIdiff > 0] <- 1
  both$tracker <- na.locf(both$tracker)
  
  frontRelevant <- both[both$tracker==1, c(1:6)]
  backRelevant <- both[both$tracker==-1, c(7:12)]
  colnames(frontRelevant) <- colnames(backRelevant) <- c("Open","High","Low","Close","Volume","OI")
  relevant <- rbind(frontRelevant, backRelevant)
  relevant[relevant==0] <- NA
  
  #remove any incomplete days, print a message saying how many removed days, print them if desired
  instrument <- gsub("CHRIS/", "", stemCode)
  relevant$Open[is.na(relevant$Open)] <- relevant$Close[(which(is.na(relevant$Open))-1)]
  NAs <- which(is.na(relevant$Open) | is.na(relevant$High) | is.na(relevant$Low) | is.na(relevant$Close))
  if(verbose) {
    if(verbose) { message(paste(instrument, "had", length(NAs),"incomplete days removed from data.")) }
    print(relevant[NAs,])
  }
  if(length(NAs) > 0) {   
    relevant <- relevant[-NAs,]
  }
  
  relevant$ATR <- ATR(HLC=HLC(relevant))$atr
  #Technically somewhat cheating, but could be stated in terms of lag 2, 1, and 0.
  #A spike is defined as a data point on Close that's more than 5 ATRs away from both
  #The preceding and following day.
  spikes <- which(abs((relevant$Close-lag(relevant$Close))/relevant$ATR) > 5 
                  & abs((relevant$Close-lag(relevant$Close, -1))/relevant$ATR) > 5)
  if(verbose) {
    message(paste(instrument, "had", length(spikes),"spike days removed from data."))
    print(relevant[spikes,])
  }
  
  if(length(spikes) > 0){
    relevant <- relevant[-spikes,]
  }
  
  intraDaySpikes <- which((relevant$High-relevant$Low)/relevant$ATR > 10 | 
                            relevant$Open > relevant$High | relevant$Close > relevant$High |
                            relevant$Open < relevant$Low | relevant$Close < relevant$Low)
  if(verbose) {
    message(paste(instrument, "had", length(intraDaySpikes), "intraday spikes removed from data."))
    print(relevant[intraDaySpikes,])
  }
  
  if(length(intraDaySpikes) > 0) {
    relevant <- relevant[-intraDaySpikes,]
  }
  relevant$ATR <- NULL
  
  out <- relevant
  return(out)
}