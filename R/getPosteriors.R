#############################################################################################################
## This function takes the list created by readDMData(),
##    which has inputs from the A&P file plus user specified values.
##    priors is a list to spec the priors.  See createBUGSdata.R
## This is used then to run the code get the posteriors for the SR parameters.
## The output is written to a .csv file and an .Rdata file
## Is going to write a file, mod1.txt to the user's working directory
#############################################################################################################
getPosteriors = function(dat, input, priors=NULL, fun="jags"){
  result <- findOptimum(dat,input)
  bdat <- createBUGSdata(dat,input, priors=priors)
  calcInits <- calcInitVals(result, dat, input)
  writeBUGScode(input)
  if(fun=="jags"){
    x <- runJAGS(bdat, calcInits, nMult=10)
  }else{
    x <- runBUGS(bdat, calcInits, nMult=10)
  }
  tDat <- calculatePEandSmsy(input, dat, x)
  
  return(list(result=result, tDat=tDat))
}