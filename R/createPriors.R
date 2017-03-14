#' @title Create a priors list.
#' @param pMode Mode of the production prior
#' @param pSig Sigma of the production prior
#' @param pMin Lowerbound of the production prior
#' @param pMax Upperbound of the production prior
#' @param cMode Mode of the log capacity prior
#' @param cSig Sigma of the log capacity prior
#' @param cMin Lowerbound of the log capacity prior
#' @param cMax Upperbound of the log capacity prior
#' @param msMin Lowerbound of the marine survival coefficient prior (note ms covariate is log transformed in the code)
#' @param flowMax Upperbound of the flow coefficient prior; the coefficient is always negative  (note flow covariate is log transformed in the code)
#' @return a list with parameters for each prior.
createPriors <- function(
  pMode=20, pSig=7, pMin=0, pMax=40,
  cMu=9, cSig=50, cMin=3, cMax=12,
  msMode=0.0001, msMin=0.0001, msMax=1000, 
  flowMode=-0001, flowMin=-1000, flowMax=-0.0001){
  # reparameterize in terms of mean and precision
  if(flowMin>flowMax) stop("flowMin must be less than flowMax")
  if(flowMode>flowMax) stop("flowMode between flowMin and flowMax")
  if(flowMode<flowMin) stop("flowMode between flowMin and flowMax")
  if(msMin>msMax) stop("msMin must be less than msMax")
  if(msMode>msMax) stop("msMode must be between msMin and msMax")
  if(msMode<msMin) stop("msMode must be between msMin and msMax")
  if(cMin>cMax) stop("cMin must be less than cMax")
  if(pMin>pMax) stop("pMin must be less than pMax")
  if(cSig<0) stop("cSig must be > 0")
  if(pSig<0) stop("pSig must be > 0")

  logMu <- log(pMode)+pSig^2
  pTau <- 1/(pSig^2)
  cTau <- 1/(cSig^2)
  list(
    prodPrior = c(logMu=logMu, tau=pTau, lowerBound=pMin, upperBound=pMax),
    logCapPrior = c(mu=cMu, tau=cTau, lowerBound=cMin, upperBound=cMax),
    msCoefPrior = c(mu=msMode, tau=1/(10*10), lowerBound=msMin, upperBound=msMax),
    flowCoefPrior = c(mu=flowMode, tau=1/(10*10), lowerBound=flowMin, upperBound=flowMax)
  )
}

