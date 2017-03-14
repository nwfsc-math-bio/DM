#' @title Original dynamic model
#' 
#' @description Implements the DM model from the original RER process. Estimates escapement by year using escapement data from previous years, maturation rates, harvest rates, and rates of natural mortality. The estimated escapement is log transformed and subtracted from the log transformed values of observed escapement. These differences are then squared and summed to get the sum of squares residual which is returned by the function. This function can be used to estimated the spawner-recruit parameters through the use of an optimization function. 
#' 
#' @param p The values of the SR function parameters. p[1] and p[2] are logged to keep them positive.  p[1]=log(prod), p[2]=log(cap).  The other p depend on what covariates are in the model.  These are unlogged as they could be positive or negative depending on the covariate.. The length of p is 2 + number of covariates.  If there are 2 covariates, then p[3]= msCoef and p[4]=flowCoef.
#' @param SRfunc a SR function: ricker, bevertonHolt, hockeyStick, as returned by selectSR()
#' @param dat data from the A & P file
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM", SRfunction, covariates, includeMarineSurvival, includeFlow
#' @param returnAll (TRUE/FALSE)
#' 
#' @return Returns the sum of squares residuals between the observed and estimated escapement. If returnAll is true, the estimated and observed escapement are also returned.
SSTfuncDM <- function(p, SRfunc, dat, input, returnAll=FALSE){
  # create centered covariates if necessary
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  flowVar <- if(input$centerFlow & input$includeFlow=="yes") log(dat$flow) - logFlowMu else log(dat$flow)
  msVar <- if(input$centerMS & input$includeMarineSurvival=="yes") log(dat$marineSurvivalIndex) - logMSMu else log(dat$marineSurvivalIndex)
  covariates=list()
  if(input$includeMarineSurvival=="yes") covariates=c(covariates,list(marineInd=msVar))
  if(input$includeFlow=="yes") covariates= c(covariates, list(flow=flowVar))
  
  # create matrices
  postMortCohort <- array(NA,dim=c(length(dat$broodYear),4))
  matureRun <- array(NA,dim=c(length(dat$broodYear),4))
  postMaturationCohort <- array(NA,dim=c(length(dat$broodYear),4))	
  # spawner-recruit function to generate age 2 fish
  # using totalSpawners as opposed to wildTotalSpawners
  # (to account for in river hatchery fish spawning. So that you are not over optimistic in your estimates)
  # spawner-recruit function
  # Note that p matches the number of covariates in the covariates list.  If no covariates, p is length 2.
  AEQrecruitsFromSRrelationship <- SRfunc(S=dat$totalSpawnersAge3to5, covariates, p)
  # adjust for adult equivalents (convert Age 5 to Age 2) so SR function can be in terms of adults
  AEQrecruitsFromExpansionFactor <- AEQrecruitsFromSRrelationship / (dat$AEQ[,1] * (1 - input$naturalMort[2]))
  # account for natural mort and fishing
  postMortCohort[,1] <- AEQrecruitsFromExpansionFactor * (1 - input$naturalMort[2]) * (1 - dat$mixedMaturityFishingRate[,1])
  # mature
  matureRun[,1] <- postMortCohort[,1]*dat$maturationRate[,1]
  # what's left
  postMaturationCohort[,1] <- postMortCohort[,1] - matureRun[,1]
  # age 3-5 built on age 2 fish 
  for(i in 2:4){
    postMortCohort[,i] <- postMaturationCohort[,i-1] * (1 - input$naturalMort[i+1]) * (1 - dat$mixedMaturityFishingRate[,i])
    matureRun[,i] <- postMortCohort[,i]*dat$maturationRate[,i]
    postMaturationCohort[,i] <- postMortCohort[,i] - matureRun[,i]
  }
  # estimate escapement for ages 3 to 5 and compare to observed escapement to get SSE
  escapement <- matureRun * (1 - dat$matureFishingRate) * (1 - dat$preSpawnMortRate)
  BY <- dat$broodYear
  years <- input$firstYear:(input$lastYear-2)
  predictedWildEscapementAge3to5 <- escapement[BY %in% (years+2),2] + escapement[BY %in% (years+1),3] + escapement[BY %in% years,4]	
  SE <- ((log(predictedWildEscapementAge3to5) - log(dat$totalWildEscapementAge3to5[BY %in% (years+5)]))^2)
  SE <- SE[1:(input$lastYear-input$firstYear-4)] # limit years to first:last
  if(!returnAll){ 
    sum(SE) 
  }else{
    list(broodYear=years+5,
         predictedWildEscapementAge3to5=predictedWildEscapementAge3to5,
         totalWildEscapementAge3to5=dat$totalWildEscapementAge3to5[BY %in% (years+5)],
         SSE = sum(SE)
    )
  }
}
