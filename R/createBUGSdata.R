#' @title Create bugs data for model with or without age data.
#' 
#' @description This function creates bugs data (bdat).
#' 
#' @param dat data from the A & P file
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM" or "SS", SRfunction, includeMarineSurvival, includeFlow
#' @param priors the parameters for the prior distributions used in the JAGS models.
#' @param allData if allData=TRUE the data for both model types is returned. If allData=FALSE, just the data necessary for the specified model is returned.
#' @return dataframe returns a list with the data and priors formatted as input for the JAGS model.
createBUGSdata <- function(dat, input, priors=NULL, allData=FALSE){
  
  estimateMaturation <- ifelse(input$analysisType=="SS", TRUE, FALSE)
  yrInd <- which(dat$broodYear==input$firstYear):which(dat$broodYear==(input$lastYear))
  yrInd_p3 <- which(dat$broodYear==input$firstYear):(which(dat$broodYear==(input$lastYear))+3)
  yrInd_age <- (which(dat$broodYear==(input$firstYear))+5):(which(dat$broodYear==(input$lastYear))+2) # changed from 3 to 2
  BY <- dat$broodYear[yrInd]
  BYage <- dat$broodYear[yrInd_p3]
  
  # prepare age data
  ageDat <- dat$ages[yrInd_age,]
  notNA <- !is.na(ageDat[,1])
  aDat <- list()
  aDat$A <- ageDat[notNA,]
  aDat$AgeYears <- dat$broodYear[yrInd_age][notNA]
  aDat$Asum <- apply(aDat$A,1,sum)
  aDat$aYears <- dim(aDat$A)[1]
  
  sharedDat <- list(
    firstYear = 1,
    lastYear = length(BY),
    naturalMort = input$naturalMort,
    matureFishingRate = as.matrix(dat$matureFishingRate)[yrInd,],
    mixedMaturityFishingRate = as.matrix(dat$mixedMaturityFishingRate)[yrInd,],
    preSpawnMortRate = as.matrix(dat$preSpawnMortRate)[yrInd,],
    wildEscapementAge3to5obs = dat$totalWildEscapementAge3to5[yrInd_p3],
    pHOS = (dat$totalSpawnersAge3to5[yrInd]-dat$totalWildEscapementAge3to5[yrInd])/dat$totalSpawnersAge3to5[yrInd]
  )
  
  # add marine survival and flow data if necessary
  if(input$includeMarineSurvival=="yes"){
    MSind <- log(dat$marineSurvivalIndex[yrInd])
    if(input$centerMS) sharedDat <- c(sharedDat,list(logMarineSurvivalIndex = MSind-mean(MSind,na.rm=TRUE)))
    else sharedDat <- c(sharedDat,list(logMarineSurvivalIndex = MSind))
  }
  if(input$includeFlow=="yes"){
    flowInd <- log(dat$flow[yrInd])
    if(input$centerFlow) sharedDat <- c(sharedDat,list(logFlow = flowInd-mean(flowInd,na.rm=TRUE)))
    else sharedDat <- c(sharedDat,list(logFlow = flowInd))
  }
  
  # data just necessary for the DM model
  DMdat <- list(
    escapementAge3to5obs = dat$totalSpawnersAge3to5[yrInd],
    AEQ = dat$AEQ[yrInd,1],
    maturationRate = as.matrix(dat$maturationRate[yrInd,]),
    AEQR = dat$AEQR
  )
  
  # data just neccesary for the State Space model
  SSdat <- list(
    A = aDat$A,
    AgeYears = which(BYage %in% aDat$AgeYears),
    aYears = aDat$aYears,
    Asum = as.vector(aDat$Asum)
    #obsSDval = 0.15 # this is only used if observation error is fixed
    )
  
  # create base priors. Use priors parameter if non-null
  if(is.null(priors)){
    priors <- createPriors()
  }else{
    # expecting a list with prodPrior, logCapPrior,msCoefPrior, and flowCoefPrior
    priorNames <- c("prodPrior","logCapPrior","msCoefPrior","flowCoefPrior")
    if(!all(priorNames %in% names(priors))){
      stop("ERROR: missing prior in createBUGSdata.")
    }
    # fill in any missing prior values with defaults
    defaultPriors <-   createPriors()
    for(iName in names(priors)){
      for(jName in names(priors[[iName]]))
        defaultPriors[[iName]][jName] <- priors[[iName]][jName]
    }
    priors <- defaultPriors
    
    # error checking the priors
    if(priors$flowCoefPrior["lowerBound"]>priors$flowCoefPrior["upperBound"])
      stop("lowerbound of flow coef prior must be less than upperbound")
    if(priors$msCoefPrior["lowerBound"]>priors$msCoefPrior["upperBound"])
      stop("lowerbound of ms coef prior must be less than upperbound")
    if(priors$logCapPrior["lowerBound"]>priors$logCapPrior["upperBound"])
      stop("lowerbound of capacity prior must be less than upperbound")
    if(priors$prodPrior["lowerBound"]>priors$prodPrior["upperBound"])
      stop("lowerbound of production prior must be less than upperbound")
    if(priors$logCapPrior["tau"]<=0) stop("sigma of capacity prior must be > 0")
    if(priors$prodPrior["tau"]<=0) stop("sigma of production prior must be > 0")
    if(priors$flowCoefPrior["tau"]<=0) stop("sigma of flow coef prior must be > 0")
    if(priors$msCoefPrior["tau"]<=0) stop("sigma of marine survival coefficient prior must be > 0")

    
  }
  
  if(allData){
    bdat <- c(priors,sharedDat,SSdat,DMdat)
  }else{
    if(input$analysisType=="SS"){
      bdat <- c(priors,sharedDat,SSdat)
    }else{
      bdat <- c(priors,sharedDat,DMdat)
    }
  }
  bdat
}
# tt$input$analysisType <- "SS"  
# createBUGSDM_AgeData(tt$dat,tt$input,basePriors=NULL)
