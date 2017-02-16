#############################################################################################################
## This function call bugs to get the posterior draws
#############################################################################################################
runJAGS <- function(bdat, calcInits, nMult=10){
  saveList <- c("prod","logCap","msCoef","flowCoef","tau")
  m1 <- jags(data=bdat, inits=calcInits, parameters.to.save=saveList, model.file="mod1.txt",
             n.chains=1, n.iter=1100*nMult, n.burnin=100*nMult, n.thin=nMult, 
             DIC=TRUE, digits=5)
  m1$BUGSoutput$sims.list
}