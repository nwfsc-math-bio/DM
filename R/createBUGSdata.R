#############################################################################################################
## This function takes the dat and input from the A&P file and creates bdat list needed by runBugs
#############################################################################################################
createBUGSdata <- function(dat,input,priors=NULL){
  centerVars <- F
  BUGsDat <- list(
    firstYear = which(dat$broodYear==input$firstYear),
    lastYear = which(dat$broodYear==(input$lastYear)),
    AEQ = dat$AEQ[,1],
    naturalMort = input$naturalMort,
    matureFishingRate = as.matrix(dat$matureFishingRate),
    mixedMaturityFishingRate = as.matrix(dat$mixedMaturityFishingRate),
    maturationRate = as.matrix(dat$maturationRate),
    preSpawnMortRate = as.matrix(dat$preSpawnMortRate),
    totalSpawnersAge3to5 = dat$totalSpawnersAge3to5,
    logTotalWildEscapementAge3to5 = log(dat$totalWildEscapementAge3to5)
  )
  if(input$includeMarineSurvival=="yes"){
    if(centerVars) BUGsDat <- c(BUGsDat,list(marineSurvivalIndex = dat$marineSurvivalIndex-mean(dat$marineSurvivalIndex,na.rm=T)))
    else BUGsDat <- c(BUGsDat,list(marineSurvivalIndex = dat$marineSurvivalIndex))
  }
  if(input$includeFlow=="yes"){
    if(centerVars) BUGsDat <- c(BUGsDat,list(flow = dat$flow-mean(dat$flow,na.rm=T)))
    else BUGsDat <- c(BUGsDat,list(flow = dat$flow))
  }
  
  # create base priors
  if(is.null(priors)){
    pMode <- 3
    pSig <- 3
    logMu <- log(pMode)+pSig^2
    pTau <- 1/(pSig^2)
    cTau <- 1/(50^2)
    priors <- list(
      prodPrior = c(logMu=logMu,tau=pTau,lowerBound=0,upperBound=20),
      logCapPrior = c(mu=9,tau=cTau,lowerBound=3,upperBound=12),
      msCoefPrior = c(lowerBound=0,upperBound=100),
      flowCoefPrior = c(lowerBound=0,upperBound=100),
      tauPrior = c(gamma1=0.0001,gamma2=0.0001)
    )
  }else{
    # expecting a list with prodPrior, logCapPrior,msCoefPrior, flowCoefPrior, and tauPrior
    # see above for paramterizations
    priorNames <- c("prodPrior","logCapPrior","msCoefPrior","flowCoefPrior","tauPrior")
    if(!all(priorNames %in% names(priors))){
      stop("ERROR: missing prior in createBUGSdata.")
    }
  }
  
  bdat <- c(priors,BUGsDat)
  bdat
}