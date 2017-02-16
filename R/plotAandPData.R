plotAandPData= function(RData.file){
  load(RData.file)
  # plot of the original data
  par(mfrow=c(3,2))
  plot(dat$broodYear,dat$totalSpawnersAge3to5,type="l", xlab="",ylab="total spawners",main="Total Spawners age 3 to 5")
  plot(dat$broodYear,dat$totalWildEscapementAge3to5,type="l", xlab="",ylab="total wild esc.",main="Total Wild Esc. age 3 to 5")
  matplot(dat$broodYear,dat$preSpawnMortRate,type="p", xlab="",ylab="mort. rate",main="pre-spawning mort. rate")
  matplot(dat$broodYear,dat$maturationRate,type="p", xlab="",ylab="mat. rate",main="maturation rate")
  matplot(dat$broodYear,dat$matureFishingRate,type="p", xlab="",ylab="fish. rate",main="Mature Fish. Rate")
  matplot(dat$broodYear,dat$mixedMaturityFishingRate,type="p", xlab="",ylab="fish. rate",main="Mixed Mat. Fish. Rate")
  invisible("Data from the A and P files.")
}