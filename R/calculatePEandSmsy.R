#############################################################################################################
## This function takes input and dat produced from the A & P files along with x (the posteriors) from
## getPosteriors().  It computes the process error estimates (MSE and autoCorr or mean and sterr) along
## with the SMSY for each posterior draw.  It then assembles all these into a dataframe tDat and
## returns that.
#############################################################################################################
calculatePEandSmsy <- function(input,dat,x){
  # n = number of sims
  n <- length(x$prod)
  # transform to agree with parameterization used in A&P (and opt procedures here)
  tDat <- data.frame(a=x$prod,b=exp(x$logCap),c=x$msCoef,d=-x$flowCoef)
  # if covariates are included then you calculate MSE and autocorr (otherwise calc gamma params)
  if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){
    # calculate the MSE and autocorr for the recruit residuals (only do this for 3 and 4 param models otherwise use gamma dist)
    MSE <- numeric(n)
    autoCorr <- numeric(n)
    for(i in 1:n){
      xx <- selectSR(input$SRfunc)(dat$totalSpawnersAge3to5,dat$flow,dat$marineSurvivalIndex,c(log(x$prod[i]),x$logCap[i],log(x$msCoef[i]),log(x$flowCoef[i])))
      yy <- ((log(dat$AEQR) - log(xx))^2)[dat$broodYear %in% (input$firstYear:input$lastYear)]
      yy <- yy[!is.na(yy)]
      nn <- length(yy)
      MSE[i] <- sum(yy)/(nn-4)
      autoCorr[i] <- cor(yy[-nn],yy[-1])
    }
    tDat$MSE <- MSE
    tDat$autoCorr <- autoCorr
    # calculate Smsy
    meanInd <- numeric(n)
    yrInd <- which(dat$broodYear %in% input$MSYfirstYear:input$MSYlastYear) # years used to calculate average Environmental Conditions (not always the same as first and last Year)
    for(i in 1:n){
      meanInd[i] <- mean(exp(-x$flowCoef[i]*dat$flow[yrInd])*dat$marineSurvivalIndex[yrInd]^x$c[i])
    }
    # to use the Smsy functions with flow and marine survival we need to adjust 
    #   prod for all SR funcs but cap for only BH and HS (not Ricker)
    if(input$SRfunc %in% c("bevertonHolt","hockeyStick")) newCap <- exp(tDat$logCap)*meanInd
    newProd <- tDat$prod*meanInd
    tDat$Smsy <- selectSmsy(input$SRfunc)(newProd,newCap)  
  } else {
    # calculate the posterior distribution for the gamma parameters for the SR relationship (only for 2 parameter models)
    meanSR <- numeric(n)
    sdSR <- numeric(n)
    for(i in 1:n){
      xx <- selectSR(input$SRfunc)(dat$totalSpawnersAge3to5,dat$flow,dat$marineSurvivalIndex,c(log(x$prod[i]),x$logCap[i],log(x$msCoef[i]),log(x$flowCoef[i])))
      yy <- (dat$AEQR/xx)[dat$broodYear %in% (input$firstYear:input$lastYear)]
      meanSR[i] <- mean(yy,na.rm=T)
      sdSR[i] <- sd(yy,na.rm=T)
    }
    tDat$meanSR <- meanSR
    tDat$sdSR <- sdSR
    # calculate Smsy
    tDat$Smsy <- selectSmsy(input$SRfunc)(x$prod,exp(x$logCap))
  }
  tDat
}