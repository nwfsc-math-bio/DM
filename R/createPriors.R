#' @title Create a priors list.
#' @param pMode Mode of the production prior (dlnorm)
#' @param pSig Sigma of the production prior
#' @param pMin Lowerbound of the production prior
#' @param pMax Upperbound of the production prior
#' @param cMu Mean of the log capacity prior (dnorm)
#' @param cSig Sigma of the log capacity prior
#' @param cMin Lowerbound of the log capacity prior
#' @param cMax Upperbound of the log capacity prior
#' @param msMu Mean of the marine survival coef prior (dnorm)
#' @param msSig Sigma of the marine survival coef prior
#' @param msMin Lowerbound of the marine survival coefficient prior
#' @param msMax Upperbound of the marine survival coefficient prior
#' @param flowMu Mean of the marine survival coef prior (dnorm)
#' @param flowSig Sigma of the marine survival coef prior
#' @param flowMin Lowerbound of the flow coefficient prior; the coefficient is always negative
#' @param flowMax Upperbound of the flow coefficient prior; the coefficient is always negative
#' @return a list with parameters for each prior.
createPriors <- function(
  pMode=20, pSig=7, pMin=0, pMax=40,
  cMu=9, cSig=50, cMin=3, cMax=12,
  msMu=0.0001, msSig=10, msMin=0.0001, msMax=1000, 
  flowMu=-.0001, flowSig=10, flowMin=-1000, flowMax=-0.0001){
  # reparameterize in terms of mean and precision
  if(flowMin>flowMax) stop("flowMin must be less than flowMax")
  if(flowMu>flowMax) stop("flowMu must be between flowMin and flowMax")
  if(flowMu<flowMin) stop("flowMu must be between flowMin and flowMax")
  if(msMin>msMax) stop("msMin must be less than msMax")
  if(msMu>msMax) stop("msMu must be between msMin and msMax")
  if(msMu<msMin) stop("msMu must be between msMin and msMax")
  if(cMin>cMax) stop("cMin must be less than cMax")
  if(pMin>pMax) stop("pMin must be less than pMax")
  if(cSig<0) stop("cSig must be > 0")
  if(pSig<0) stop("pSig must be > 0")
  if(msSig<0) stop("msSig must be > 0")
  if(flowSig<0) stop("flowSig must be > 0")
  
  logMu <- log(pMode)+pSig^2
  pTau <- 1/(pSig^2)
  cTau <- 1/(cSig^2)
  msTau <- 1/(msSig^2)
  flowTau <- 1/(flowSig^2)
  list(
    prodPrior = c(logMu=logMu, tau=pTau, lowerBound=pMin, upperBound=pMax),
    logCapPrior = c(mu=cMu, tau=cTau, lowerBound=cMin, upperBound=cMax),
    msCoefPrior = c(mu=msMu, tau=msTau, lowerBound=msMin, upperBound=msMax),
    flowCoefPrior = c(mu=flowMu, tau=flowTau, lowerBound=flowMin, upperBound=flowMax)
  )
}

