#' @title Save DM output.
#' 
#' @description Write DM output (posteriors) to csv file and RData file.
#' 
#' @param population Name of the population.
#' @param input a list with the other values needed for a DM run. The following are the defaults used, but can be passed in to specify something different: naturalMort = c(0.5,0.4,0.3,0.2,0.1,0), analysisType = "DM", SRfunction = "ricker", covariates = "no", includeMarineSurvival = "no", includeFlow = "no"
#' @param dat data from the A & P file
#' @param tDat a data frame of the posteriors and calculated process error and Smsy for each draw.  From calculatePEandSmsy(). 
#' @param mlEst The maximum likelihood estimate for the four SR parameters
#' @param bdat data for the bayesian specification of a DM run
#' @param filename name to give the outputfiles file
#' 
#' @return nothing is returned but the csv and RData files are written.
writeResultsToFile <- function(input, dat, tDat, mlEst, bdat, plist, filename){
  file <- filename
  baseFileName = filename
  if(str_sub(file,-4)==".csv") baseFileName = str_sub(file,1,-5)
  if(str_sub(file,-4)==".rda") baseFileName = str_sub(file,1,-5)
  if(str_sub(file,-6)==".RData") baseFileName = str_sub(file,1,-7)
  
  write.csv(tDat, file=paste(baseFileName, ".csv", sep=""), row.names=FALSE)
  save(input, dat, mlEst, tDat, bdat, plist,
       file=paste(baseFileName, ".RData", sep=""))
}