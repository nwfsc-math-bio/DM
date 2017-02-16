#############################################################################################################
## This function call bugs to get the posterior draws
#############################################################################################################
runBUGS <- function(bdat, calcInits, nMult=10){
  saveList <- c("prod","logCap","msCoef","flowCoef","tau")
  m1 <- bugs(data=bdat, inits=calcInits, parameters.to.save=saveList, model.file="mod1.txt",
             n.chains=1, n.iter=1100*nMult, n.burnin=100*nMult, n.thin=nMult, 
             debug=F, DIC=TRUE, digits=5, over.relax = T)
  
  m1$sims.list
}