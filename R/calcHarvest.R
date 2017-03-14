#' @title Calculate harvest rate
#' @description Calculate the harvest rate by year and age of return
#' @param bdat data for the bayesian specification of a DM run
#' @return Harvest rate (scalar).
calcHarvest <- function(bdat){
  notH <- array(NA,dim=dim(bdat$matureFishingRate))
  for(a in 1:4){
    tmp <- rep(1,dim(notH)[1])
    for(i in 1:a){
      tmp <- tmp*(1-bdat$mixedMaturityFishingRate[,i])
    }
    notH[,a] <- tmp*(1-bdat$matureFishingRate[,a])
  }
  1-notH
}

