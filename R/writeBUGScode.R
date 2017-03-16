#' @title write the model in BUGS language
#' 
#' @description create a mod1.txt which contains the model in the BUGS language
#' 
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM", SRfunction, covariates, includeMarineSurvival, includeFlow
#' @param outputText (TRUE/FALSE)
#' 
#' @return nothing but writes the file mod1.txt which is read in to run the BUGS model
#' @details Note that the flow coefficient is always negative and prior set as a normal with upper (negative bound)
writeBUGScode <- function(input=NULL, outputText=FALSE){  
  
  if(is.null(input)){
    mod <- list(
      srFunc="ricker",
      includeMarineSurvival="no",
      includeFlow="no",
      useAEQrecruits=TRUE,
      stateSpace=FALSE,
      estimateMaturation=TRUE,
      fixedObsError=NULL
      )
  }else{
    SS <- input$analysisType=="SS"
    mod <- list(
      srFunc=input$SRfunction,
      includeMarineSurvival=input$includeMarineSurvival,
      includeFlow=input$includeFlow,
      useAEQrecruits=TRUE,
      AEQmean=SS,
      stateSpace=SS,
      estimateMaturation=SS,
      fixedObsError=input$escapementObsSD,
      age2correction=0.5
      )
  }
    
  # Define SR function
  tmpStr <- switch(mod$srFunc,
                   hockeyStick = "min(prod*escapementAge3to5[year],exp(logCap))",
                   ricker = "prod*escapementAge3to5[year]*exp(-escapementAge3to5[year]/exp(logCap))",
                   bevertonHolt = "escapementAge3to5[year]/(escapementAge3to5[year]/exp(logCap) + 1/prod)"
  ) 
  SRstr <- paste(tmpStr,ifelse(mod$includeMarineSurvival=="yes","*exp(msCoef*logMarineSurvivalIndex[year])",""),  
                        ifelse(mod$includeFlow=="yes",          "*exp(flowCoef*logFlow[year])",""),sep="")
  
  # divide by AEQ?
  if(mod$useAEQrecruits){
    if(mod$estimateMaturation){
      if(mod$AEQmean){
        AEQdiv <- "/(AEQm[1]*(1 - naturalMort[2]))"
      }else{
        AEQdiv <- "/(AEQ[year,1]*(1 - naturalMort[2]))" 
      }
    }else{
      AEQdiv <- "/(AEQ[year]*(1 - naturalMort[2]))" 
    }
  }else{
    AEQdiv <- ""
  } 
  
  # state space model?
  if(mod$stateSpace){
    SS1 <- "escapementAge3to5[year] <- wildEscapementAge3to5[year]/(1-pHOS[year])"
    SS2 <- "~ dlnorm(log(predictedRecruits[year]),procTau)"
  }else{
    SS1 <- "escapementAge3to5[year] <- escapementAge3to5obs[year]"
    SS2 <- "<- predictedRecruits[year]"
  }
  # write model to file
  modelText <- paste("
model
{
		
  for(year in firstYear:lastYear){
    # generate age 2 values
    ",SS1,"
    predictedRecruits[year] <- ",SRstr,"
    recruits[year] ",SS2,"
    smolt[year] <- recruits[year]",AEQdiv,"
    postMortCohort[year,1] <- smolt[year] * (1 - naturalMort[2]) * (1 - mixedMaturityFishingRate[year,1])
    matureRun[year,1] <- postMortCohort[year,1]*maturationRate[year,1]
    postMaturationCohort[year,1] <- postMortCohort[year,1] - matureRun[year,1]
    escapement[year,1] <- matureRun[year,1] * (1 - matureFishingRate[year,1]) * (1 - preSpawnMortRate[year,1])
    # age 3-5 built on age 2 fish 
    for(age in 2:4){
      postMortCohort[year,age] <- postMaturationCohort[year,age-1] * (1 - naturalMort[age+1]) * (1 - mixedMaturityFishingRate[year,age])
      matureRun[year,age] <- postMortCohort[year,age]*maturationRate[year,age]
      postMaturationCohort[year,age] <- postMortCohort[year,age] - matureRun[year,age]
      escapement[year,age] <- matureRun[year,age] * (1 - matureFishingRate[year,age]) * (1 - preSpawnMortRate[year,age])
    }
  }	

  # escapement observation model data
  for(year in (firstYear+5):(lastYear+3)){
    wildEscapementAge3to5obs[year] ~ dlnorm(log(wildEscapementAge3to5[year]),obsTau)
    wildEscapementAge3to5[year] <- escapement[year-3,2] + escapement[year-4,3] + escapement[year-5,4]
  }

  # fill in first years so I can monitor the node (and to initialize for state space model)
  for(year in 1:(firstYear+4)){
    wildEscapementAge3to5[year] <- wildEscapementAge3to5obs[year]
  }

",ifelse(mod$estimateMaturation,paste("

  # age composition data
  for(i in 1:aYears){
    A[i,1:4] ~ dmulti(App[i,1:4],Asum[i]) #-A[i,1])  # should probably include something here to allow for over dispersion
    ApSum[i] <- sum(Ap[i,1:4])
    Ap[i,1] <- escapement[AgeYears[i]-2, 1]*",mod$age2correction," # correct for lower prob of seeing age 2 fish
    App[i,1] <- Ap[i,1]/ApSum[i] 
    for(age in 3:5){ 
      # ages 2:5 predicted with escapement. This assumes the hatchery and natural age composition are comparable
      Ap[i,age-1] <- escapement[AgeYears[i]-age, age-1] 
      #Ap[i,age-1] ~ dlnorm(log(escapement[AgeYears[i]-age, age-1]),1/0.25) # ALLOW FOR OVER-DISPERSION!
      App[i,age-1] <- Ap[i,age-1]/ApSum[i]
    }
  }

  # priors for r (maturation) assume r changes from year to year but comes from a common distribution
  for(year in firstYear:lastYear){
    for(age in 2:4){
      # here rMu and rTau are on the un transformed scale -inf to +inf  
      # so 0 = 0.5 on the transformed scale and 1 = 0.73 and 5 = 0.99
      logit(maturationRate[year,age-1]) <- max(min(maturationRate_logit[year,age-1],5),-5) # exclude extreme values
      maturationRate_logit[year,age-1] ~ dnorm(rMu[age-1],rTau[age-1])
      # AEQ adult equivalents so the spawner recruit function can be in terms of adults
      AEQ[year,age-1] <- maturationRate[year,age-1] + (1-maturationRate[year,age-1])*(1 - naturalMort[age+1])*AEQ[year,age]
    }
    maturationRate[year,4] <- 1 # all remaining fish mature at age 5 (not completely true but more or less)
    AEQ[year,4] <- maturationRate[year,3] + (1-maturationRate[year,3])*(1 - naturalMort[6])
  }

  # create AEQ based on median maturation rates
  for(age in 2:4){
    logit(meanMat[age-1]) <- rMu[age-1]
    AEQm[age-1] <- meanMat[age-1] + (1-meanMat[age-1])*(1 - naturalMort[age+1])*AEQm[age]
  }
  meanMat[4] <- 1
  AEQm[4] <- meanMat[3] + (1-meanMat[3])*(1 - naturalMort[6])
    
  # hyper priors for rMu and rSD / rTau
  for(age in 2:4){
    rMu[age-1] ~ dnorm(0,0.01)
    rTau[age-1] <- 1/(rSD[age-1]*rSD[age-1])
    rSD[age-1] ~ dunif(0.01,3)
  }
  ",sep=""),""),"

  # priors
  prod ~ dlnorm(prodPrior[1],prodPrior[2])I(prodPrior[3],prodPrior[4])
  logCap ~ dnorm(logCapPrior[1],logCapPrior[2])I(logCapPrior[3],logCapPrior[4])
  msCoef ~ dnorm(msCoefPrior[1],msCoefPrior[2])I(msCoefPrior[3],msCoefPrior[4])
  flowCoef ~ dnorm(flowCoefPrior[1],flowCoefPrior[2])I(flowCoefPrior[3],flowCoefPrior[4])
  obsTau <- 1/(obsSD*obsSD)
  ",ifelse(!is.null(mod$fixedObsError),paste("obsSD <-",mod$fixedObsError),"obsSD ~ dt(0,1,1)T(0,)"),"
  procTau <- 1/(procSD*procSD)
  procSD ~ dt(0,1,1)T(0,)
}
",sep="")
  
  write(modelText, file="mod1.txt")
  if(outputText){
    returnVal <- modelText
  }else{
    returnVal <- NULL
  }
  returnVal
}