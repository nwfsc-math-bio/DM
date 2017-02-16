########################################################################################
## Writes the output to a .csv and .Rdata file
########################################################################################
writeResultsToFile <- function(population, input, dat, tDat, result, file){
  baseFileName = file
  if(str_sub(file,-4)==".csv") baseFileName = str_sub(file,1,-5)
  if(str_sub(file,-4)==".rda") baseFileName = str_sub(file,1,-5)
  if(str_sub(file,-6)==".RData") baseFileName = str_sub(file,1,-7)
  
  write.csv(tDat, file=paste(baseFileName, ".csv", sep=""), row.names=FALSE)
  save(population, input, dat, result, tDat, file=paste(baseFileName, ".RData", sep=""))
}