#' @title Run the DM model
#' @description This function takes the list created by readDMData() and runs
#' the Bayesian model to estimate the SR parameters.
#' The output is written to a .csv file and an .Rdata file.
#' It writes a file, mod1.txt to the user's working directory
#' @param filename csv file saved from the DM tab of A & P file.  Must have age data.
#' @param input A list that includes parameters for running the model (see readData for details) of particular interest are the parameters SRfunction, analysisType, and escapementObsSD. SRfunction sets the spawner-recruit function to be used. Current options are "ricker", "bevertonHolt" or "hockeyStick".   analysisType can be either "DM" or "SS".  DM corresponds to the traditional Dynamic Model which uses the maturation rates from the DynamicsInput tab in the A&P table. SS corresponds to the State Space model which estimates the maturation rates and includes observation error in the spawner counts.  If escapementObsSD is a positive real number, observation error is fixed at that value. If it is NULL, observation error is a free parameter that is estimated.
#' @param priors parameters for prior distributions in the JAGS model
#' @param run if run=TRUE, the model is run and the results returned, otherwise all of the information necessary to run the model is returned without running the model
#' @param sims length of each MCMC chain; default is 10000
#' @param numChains number of MCMC chains. default is 3
#' @param oldSims ?
#' @param silent Whether to print progress messages.
#' @return a dmObj: a list of output for a single model fit.
runModel <- function(filename, input=NULL,
                     priors=NULL, run=TRUE,
                     sims=10000, numChains=3,
                     oldSims=NULL, silent=FALSE){
  #readData replaces the values in input (from user) with those in the defaults in readData
  tmp <- readData(a.and.p.file=filename, input, silent=silent)
  dat <- tmp$dat
  input <- tmp$input

  mlEst <- findOptimum(dat, input)
  bdat <- createBUGSdata(dat,input,priors)
  if(!silent) cat("Finding initial conditions for Bayesian model....\n")
  calcInits <- calcInitVals(mlEst, dat, input, bdat, sims=oldSims)
  if(!silent) cat("Writing the Bayesian model code....\n")
  modelText <- writeBUGScode(input, outputText=TRUE)
  
  yrInd <- which(dat$broodYear %in% input$firstYear:(input$lastYear+3))
  if(!silent) cat("Creating the data for the Bayesian model....\n")
  bdat.all <- createBUGSdata(dat, input, priors, allData=TRUE)
  otherDat <- list(
    broodYear = dat$broodYear[yrInd],
    AEQR = dat$AEQR[yrInd],
    maturationRate = dat$maturationRate[yrInd,]
    )
  
  if(run){
    if(!silent) cat("Running JAGS....\n")
    resultVals <- runJAGS(bdat, calcInits, sims=sims, numChains=numChains, details=TRUE)
  }else{
    resultVals <- NULL
  }
  
  outList <- list(jagsOut=resultVals, bdat=bdat.all, calcInits=calcInits, input=input, 
                  dat=dat, mlEst=mlEst, otherDat=otherDat, 
                  modelText=modelText, priors=priors)
  
  if(run){
    if(!silent) cat("Preparing output list....\n")
    outList$Rhat <- resultVals$BUGSoutput$summary[,"Rhat"]
    outList$DICval <- resultVals$BUGSoutput$DIC
    outVarList <- c("prod","logCap","msCoef","flowCoef","obsSD","procSD")
    tmpT <- resultVals$BUGSoutput$summary
    outList$resultTable <- tmpT[rownames(tmpT) %in% outVarList,]
    #BUGSoutput$sims.list is the posterior draws
    outList$tDat <- calculatePEandSmsy(input, dat, resultVals$BUGSoutput$sims.list)
  }
 
  outList
}
