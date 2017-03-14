#' @title Calculate process error and SMSY.
#' @description This function takes input and dat produced from the A & P files
#' along with x (the posteriors) from getPosteriors().  It computes the process error 
#' estimates (MSE and autoCorr or mean and sterr) along with the SMSY for each posterior 
#' draw.  It then assembles all these into a dataframe tDat and returns that.
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM", SRfunction, includeMarineSurvival, includeFlow
#' @param dat data from the A & P file
#' @param postdraws posterior list from getPostDraws() call
#' @return A dataframe tDat with a column for posterior draws from each parameter 
#' and the smsy for each draw.
calculatePEandSmsy <- function(input, dat, postdraws){

  # n = number of sims
  n <- length(postdraws$prod)
  nYrs <- input$lastYear-input$firstYear+1
  AEQR <- dat$AEQR[dat$broodYear %in% (input$firstYear:input$lastYear)]
  aCorr <- function(x) cor(x[-length(x)],x[-1])
  
  # transform (uncenter) capacity and productivity if the covariates were centered in the Bayesian Analysis
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  flowCorrect <- if(input$centerFlow & input$includeFlow=="yes") exp(-postdraws$flowCoef*logFlowMu) else 1
  msCorrect <- if(input$centerMS & input$includeMarineSurvival=="yes") exp(-postdraws$msCoef*logMSMu) else 1
  prod <- postdraws$prod * flowCorrect * msCorrect
  if(input$SRfunction %in% c("bevertonHolt","hockeyStick")){
    cap <- exp(postdraws$logCap) * flowCorrect * msCorrect
  }else{
  	cap <- exp(postdraws$logCap)
  }

  # transformed (as above) to agree with parameterization used in VRAP
  tDat <- data.frame(a=prod,b=cap,c=postdraws$msCoef,d=postdraws$flowCoef)
  
  # if anaylsisType = "SS" then add avg maturation rates
  if(input$analysisType=="SS"){
    tDat[,c("mat1","mat2","mat3","mat4")] <- apply(postdraws$maturationRate,c(1,3),mean)
  }
  
  # calculate Recruitment residuals
  if(input$analysisType=="SS"){
    rResids <- log(postdraws$recruits/postdraws$predictedRecruits)
  }else{
    AEQRmat <- matrix(AEQR,byrow=TRUE,nrow=n,ncol=nYrs)
    rResids <- log(AEQRmat/postdraws$predictedRecruits) 
  }
  
  # prepare covariate adjusted parameters for calculating sMSY
  # in the bayesian model both are logged and then centered.
  # the MS index is the exponentiated again (so it is divided by it's geometric mean)
  # in VRAP the covariates are entered as: M^c exp(dF)
  # thus the unlogged ms is used; F is logged flow
  meanInd <- numeric(n)
  yrInd <- which(dat$broodYear %in% input$MSYfirstYear:input$MSYlastYear) # years used to calculate average Environmental Conditions (not always the same as first and last Year)
  yrIndMSY <- which(dat$broodYear %in% input$firstYear:input$lastYear)
  logFlowMu <- mean(log(dat$flow)[yrIndMSY])
  logMSindMu <- mean(log(dat$marineSurvivalIndex)[yrIndMSY])
  if(input$centerFlow) tmpFlow <- log(dat$flow[yrInd])-logFlowMu else tmpFlow <- log(dat$flow[yrInd])
  if(input$centerMS) tmpMSind <- exp(log(dat$marineSurvivalIndex[yrInd])-logMSindMu) else tmpMSind <- dat$marineSurvivalIndex[yrInd]
  for(i in 1:n){
    meanInd[i] <- mean(exp(postdraws$flowCoef[i]*tmpFlow)*tmpMSind^postdraws$msCoef[i])
  }
  
  # to use the Smsy functions with flow and marine survival we need to adjust 
  #   prod for all SR funcs but cap for only BH and HS (not Ricker)
  newCap <- exp(postdraws$logCap)
  if(input$SRfunc %in% c("bevertonHolt","hockeyStick")) newCap <- newCap*meanInd
  newProd <- postdraws$prod*meanInd
  
  # if covariates are included then you calculate MSE and autocorr 
  if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){
    # MSE <- postdraws$procSD^2*nYrs/(nYrs-4) (could use this instead for the SS analysis!)
    tDat$MSE <- apply(rResids,1,var)*(nYrs-1)/(nYrs-4)
    tDat$autoCorr <- apply(rResids,1,aCorr)
    # now calculate Smsy
    tDat$Smsy <- selectSmsy(input$SRfunc)(newProd,newCap)  
  # if no covariates calculate the residual mean and sd for a gamma dist fit to the residuals 
  }else{
    tDat$meanSR <- apply(exp(rResids),1,mean,na.rm=TRUE)
    tDat$sdSR <- apply(exp(rResids),1,sd,na.rm=TRUE)
    tDat$Smsy <- selectSmsy(input$SRfunc)(postdraws$prod,exp(postdraws$logCap))
  }
  
  tDat
}
