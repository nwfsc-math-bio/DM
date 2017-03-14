#' @title Calculate initial values for MCMC.
#' @description \code{calcInitVals} computes initial values for MCMC algorithm used to compute the posterior distribution for the model parameters.
#' @param mlEst The maximum likelihood estimate for the four SR parameters: log(prod), log(capacity), log(msCoef), log(flowCoef).  Output from findOptimum().
#' @param dat data from the A & P file
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM", SRfunction, includeMarineSurvival, includeFlow
#' @param bdat The in a format appropriate for JAGS implementation of the model.
#' @param sims MCMC simulations from a previous fit. If sims=NULL all initial values are based on estimates derived from the data.
#' @return A list with initial values for prod (production), logCap (log capacity), 
#' msCoef (the coefficient for marine survival), 
#' flowCoef (the coeficient for stream survival), rMu and maturationRate_logit.
calcInitVals <- function(mlEst, dat, input, bdat, sims=NULL){
  ml.estimate <- mlEst$estimate
  matRate <- array(NA,dim=c(bdat$lastYear,3))
  invLogit <- function(x) -log(1/x-1)
  for(i in 1:length(matRate[,1])) matRate[i,] <- invLogit(c(0.5,0.5,0.5))#,NA))c(0.02,0.15,0.7)
  if(is.null(sims)){
    retFunc <- function(){ 
      list(
        prod = min(c(ml.estimate["prod"],bdat$prodPrior["upperBound"]*0.95)), # multiply by 4 because these estimates are for adult equivalents.
        logCap = max(c(min(c(log(ml.estimate["cap"]),bdat$logCapPrior["upperBound"])),
                       bdat$logCapPrior["lowerBound"])),
        msCoef = bdat$msCoefPrior["lowerBound"]+0.001, 
        flowCoef = bdat$flowCoefPrior["upperBound"]-0.001, 
        rMu = invLogit(c(0.2,0.15,0.7)),
        maturationRate_logit = matRate
      )
    }
  }else{
    nn <- length(sims$prod)
    retFunc <- function(){
      list(
        prod = sims$prod[nn],
        logCap = sims$logCap[nn],
        msCoef = bdat$msCoefPrior["lowerBound"]+0.001,
        flowCoef = bdat$flowCoefPrior["upperBound"]-0.001,
        rMu = invLogit(c(0.2,0.15,0.7)),
        maturationRate_logit = invLogit(sims$maturationRate[nn,,1:3])
        #escapement = sims$escapement[nn,,]
        )
    }
  }
  retFunc
}
