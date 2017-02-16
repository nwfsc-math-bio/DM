########################################################################################
## This reads in the csv file saved from an A & P file
## srFunc and covariates are needed from the user to construct complete dat and input lists
## since those are not part of the A&P file
## The result is the dat and input ready for the GetPosteriors() function
########################################################################################
readDMData = function(a.and.p.file, srFunc, covariates, folder = "./", silent=FALSE){
  if(!silent) cat("Reading in the A and P file....\n")
  input.file=read.csv(a.and.p.file, stringsAsFactors=FALSE,header=FALSE)
  #first read in some of the data needed for inputs
  population=input.file[1,2]
  firstYear=as.numeric(input.file[4,3])
  lastYear=as.numeric(input.file[5,3])
  
  # ML added this 4/29/2015
  initialPopSize=as.numeric(str_replace_all(as.character(input.file[6,14:17]),",",""))
  
  #now trim off unneeded information to get to Martin's tDat format
  tDat=input.file[which(input.file[,1]=="Source->")+1:dim(input.file)[1],]
  tDat=tDat[,1:29]
  tDat=tDat[!is.na(tDat[,1]),]
  tDat=tDat[!(tDat[,1]==""),]
  tDat[tDat==""]=NA
  for(i in 1:dim(tDat)[2]){
    tDat[,i]=str_replace_all(tDat[,i],",","")
    tDat[,i]=as.numeric(tDat[,i])
  }
  #colnames(tDat)=NULL
  
  # entered values
  # Note natural mortality is fixed
  # a, b, c, d prelim values not used
  input <- list(
    population=population,
    naturalMort = c(0.5,0.4,0.3,0.2,0.1,0),
    firstYear = firstYear,
    lastYear = lastYear,
    MSYfirstYear = firstYear,
    MSYlastYear = lastYear,
    analysisType="DM",
    SRfunction = srFunc,
    covariates = covariates,
    includeMarineSurvival = covariates,
    includeFlow = covariates,
    initialPopSize = initialPopSize, # ML added this 4/29/2015
    prod = NA,
    cap = NA,
    msCoef = NA,
    flowCoef = NA
  )
  
  # data from A and P table
  dat <- list(
    matureFishingRate = tDat[,14:17],
    mixedMaturityFishingRate = tDat[,9:12],
    maturationRate = tDat[,19:22],
    preSpawnMortRate = tDat[,24:27],
    totalSpawnersAge3to5 = tDat[,2],
    totalWildEscapementAge3to5 = tDat[,3],
    broodYear = tDat[,1],
    AEQR = tDat[,29],
    marineSurvivalIndex = switch(covariates, no=rep(1,dim(tDat)[1]), yes=tDat[,5]),
    flow = switch(covariates, no=rep(0,dim(tDat)[1]), yes=tDat[,7])
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