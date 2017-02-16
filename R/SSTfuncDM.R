########################################################################################
## dynamic model  
########################################################################################
SSTfuncDM <- function(p,SRfunc,dat,input,returnAll=FALSE){
  # create matrices
  postMortCohort <- array(NA,dim=c(length(dat$broodYear),4))
  matureRun <- array(NA,dim=c(length(dat$broodYear),4))
  postMaturationCohort <- array(NA,dim=c(length(dat$broodYear),4))	
  # spawner-recruit function to generate age 2 fish
  # using totalSpawners as opposed to wildTotalSpawners
  # (to account for in river hatchery fish spawning. So that you are not over optimistic in your estimates)
  # spawner-recruit function
  AEQrecruitsFromSRrelationship <- SRfunc(dat$totalSpawnersAge3to5,flow=dat$flow, marineInd=dat$marineSurvivalIndex,p)
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
