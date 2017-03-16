#' @title This reads in the csv file saved from an A & P file and creates dat and input
#' 
#' @description The DM tab of the A & P excel file has all the 
#' necessary inputs to run DM.  This reads that .csv file and makes
#' a list needed for the DM functions.  creates input which has the 
#' other user specified values for the rav file.
#' 
#' @param a.and.p.file a csv file saved from the DM tab of an A & P excel file
#' @param input a list with the other values needed for a DM run. The following are the defaults used, but can be passed in to specify something different.
#' \describe{
#'   \item{population}{(from A and P file): name}
#'   \item{naturalMort}{c(0.5,0.4,0.3,0.2,0.1,0): natural mortality}
#'   \item{firstYear}{(from A and P file)}
#'   \item{lastYear}{(from A and P file)}
#'   \item{MSYfirstYear}{(same as firstYear)}
#'   \item{MSYlastYear}{(same as lastYear)}
#'   \item{analysisType}{("DM")/"SS"}
#'   \item{SRfunction}{("ricker")/"bevertonHolt"/"hockeyStick": spawner-recruit function}
#'   \item{includeMarineSurvival}{"yes"/("no"): include marine survival covariate}
#'   \item{includeFlow}{"yes"/("no"): include flow covariate}
#'   \item{initialPopSize}{(from A and P file)}
#'   \item{prod}{NA: used for initial conditions of optimizers and MCMC algorithm}
#'   \item{cap}{NA: used for initial conditions of optimizers and MCMC algorithm}
#'   \item{msCoef}{NA: used for initial conditions of optimizers and MCMC algorithm}
#'   \item{flowCoef}{NA: used for initial conditions of optimizers and MCMC algorithm}
#'   \item{centerMS}{TRUE: set mean of the covariate zero}
#'   \item{centerFlow}{TRUE: set mean of the covariate zero}
#'   \item{escapementObsSD}{NULL}
#'   \item{age2correction}{1.0: correction for seeing fewer age 2 fish when sampling for age composition. (detection probability for age 2 fish / detection probability for ages 3-5 fish) }
#' }  
#' @param silent Whether to print progress messages.
#' @return The result is a list with dat and input for the DM functions
readData = function(a.and.p.file, input, folder = "./", silent=FALSE){
  
  if(!silent) cat("Reading in the A and P file....\n")
  input.file=read.csv(a.and.p.file, stringsAsFactors=FALSE,header=FALSE)
  
  #first read in some of the data needed for inputs
  population=input.file[1,2]
  firstYear=as.numeric(input.file[4,3])
  lastYear=as.numeric(input.file[5,3])
  
  # ML added this 4/29/2015
  initialPopSize=as.numeric(str_replace_all(as.character(input.file[6,14:17]),",",""))
  
  #now trim off unneeded information
  apDat=input.file[which(input.file[,1]=="Source->")+1:dim(input.file)[1],]
  apDat=apDat[,1:42]
  apDat=apDat[!is.na(apDat[,1]),]
  apDat=apDat[!(apDat[,1]==""),]
  apDat[apDat==""]=NA
  for(i in 1:dim(apDat)[2]){
    apDat[,i]=str_replace_all(apDat[,i],",","")
    apDat[,i]=as.numeric(apDat[,i])
  }
  
  # entered values
  # Note natural mortality is fixed (due to harvest rates)
  defaultInput <- list(
    population=population,
    naturalMort = c(0.5,0.4,0.3,0.2,0.1,0),
    firstYear = firstYear,
    lastYear = lastYear,
    MSYfirstYear = firstYear,
    MSYlastYear = lastYear,
    analysisType = "DM",
    SRfunction = "ricker",
    includeMarineSurvival = "no",
    includeFlow = "no",
    initialPopSize = initialPopSize, # ML added this 4/29/2015
    prod = NA,
    cap = NA,
    msCoef = NA,
    flowCoef = NA,
    centerMS = TRUE,
    centerFlow = TRUE,
    escapementObsSD = NULL,
    age2correction = 1
  )
  
  for(iName in names(input)){
    defaultInput[iName] <- input[iName]
  }
  input <- defaultInput
  
  # data from A and P table as a list
  dat <- list(
    matureFishingRate = apDat[,14:17],
    mixedMaturityFishingRate = apDat[,9:12],
    maturationRate = apDat[,19:22],
    preSpawnMortRate = apDat[,24:27],
    totalSpawnersAge3to5 = apDat[,2],
    totalWildEscapementAge3to5 = apDat[,3],
    broodYear = apDat[,1],
    AEQR = apDat[,29],
    marineSurvivalIndex = switch(input$includeMarineSurvival, no=rep(1,dim(apDat)[1]), yes=apDat[,5]),
    flow = switch(input$includeFlow, no=rep(0,dim(apDat)[1]), yes=apDat[,7]),
    ages = round(apDat[,39:42])
  )
  
  # calculate AEQ from data and natural mortality
  # to be used to expand the age 2 recruits to age 5 recruits (assuming just natural mortality. i.e no fishing)
  AEQ <- array(NA,dim=c(length(dat$broodYear),4))
  AEQ[,4] <- dat$maturationRate[,3] + (1-dat$maturationRate[,3])*(1 - input$naturalMort[6])
  for(i in 4:2){
    AEQ[,i-1] <- dat$maturationRate[,i-1] + (1-dat$maturationRate[,i-1])*(1 - input$naturalMort[i+1])*AEQ[,i]
  }
  dat <- c(dat,AEQ=list(AEQ))
  
  return(list(dat=dat, input=input, folder=folder))
}

# fName <- "//nwcfile/CB/SalmonAPdata/Chinook_RER/dataForPriors/forMartin_justA&P_NEW/csvFiles/A&P18NisquallyV70_5-26-15_10.csv"
# fName <- "//nwcfile/CB/SalmonAPdata/Chinook_RER/dataForPriors/forMartin_justA&P_NEW/csvFiles/A&P01NFNooksackV70_WithThrowBks.csv"
# tt <- readDM_AgeData(a.and.p.file=fName)
