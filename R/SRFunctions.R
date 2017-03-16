#' @name srfunctions
#' @aliases ricker
#' @aliases bevertonHolt
#' @aliases hockeyStick
#' 
#' @title SR funcitons
#' 
#' @description Takes the S, LOG flow, LOG marine survival covariates and p parameters and returns the recruits based on the specified SR function: ricker(), bevertonHolt(), hockeyStick().  Note that it assumes that the log of the covariates are input. All covariates appear in the form exp(p*cov). p[1]=log a; p[2]=log b.  The other p are the covariate coefficients.
#' 
#' @param S spawner count
#' @param covariates A list with the covariates.  The ms covariate is normally the first and the flow covariate is the second.  Though order does not matter as the p's are matched based on order the covariate appears in the list.
#' @param p the SR function a, b, and the coefficients for each covariate in the covariate list. All covariates appear in the form exp(exp(p)*cov). p[1]=a; p[2]=b.  The other p match the covariates.
#' 
#' @return recruits (scalar)
NULL

#' @rdname srfunctions
#' @return recruits from ricker: S*exp(p[1])*exp(-S/exp(p[2]))*exp(p[4]*flow)*exp(p[3]*marineInd)
ricker <- function(S,covariates, p){ 
  val=S*exp(p[1])*exp(-S/exp(p[2]))
  if(length(covariates)==0) return(val)
  #all covariates appear in the form exp(p*cov)
  for(i in 1:length(covariates))val=val*exp(p[i+2]*covariates[[i]])
  val
}

#' @rdname srfunctions
#' @return recruits from beverton Holt: S/(S*exp(-p[2])+exp(-p[1]))*exp(p[4]*flow)*exp(p[3]*marineInd)
bevertonHolt <- function(S, covariates, p){
  val=S/(S*exp(-p[2])+exp(-p[1]))
  if(length(covariates)==0) return(val)
  #all covariates appear in the form exp(p*cov)
  for(i in 1:length(covariates))val=val*exp(p[i+2]*covariates[[i]])
  val
}

#' @rdname srfunctions
#' @return recruits from hockey stick: ifelse(exp(p[1])*S<exp(p[2]),exp(p[1])*S,exp(p[2]))*exp(p[4]*flow)*exp(p[3]*marineInd)
hockeyStick <- function(S, covariates, p){
  val=ifelse(exp(p[1])*S<exp(p[2]),exp(p[1])*S,exp(p[2]))
  if(length(covariates)==0) return(val)
  #all covariates appear in the form exp(exp(p)*cov)
  for(i in 1:length(covariates))val=val*exp(p[i+2]*covariates[[i]])
  val
}

#' @title Helper function that takes the name of SR function and returns the recruits
#' 
#' @param x name of SR function as a text string: "ricker", "bevertonHolt", "hockeyStick"
#' 
#' @return a function
selectSR <- function(x){
  if(x=="ricker") ff <- ricker
  else if(x=="bevertonHolt") ff <- bevertonHolt
  else if(x=="hockeyStick") ff <- hockeyStick
  else{
    print("ERROR: not a recognized SR function in function selectSR")
    break
  }
  ff
}

# generate function that calculates Smsy for the desired SR function 
selectSmsy <- function(funcName){
  # should change to (1-lambert_W0(exp(1-prod)))*cap # from Mark's paper (uses library gsl) (doesn't seem to work check params)
  if(funcName=="ricker") ff <- function(prod,cap) ifelse(prod>1,log(prod)*cap*(0.5-0.07*log(prod)),NA) # approximation from Hilborn
  else if(funcName=="bevertonHolt") ff <- function(prod,cap) ifelse(prod>1,cap*sqrt(1/prod)-cap/prod,NA)
  else if(funcName=="hockeyStick") ff <- function(prod,cap) ifelse(prod>1,cap/prod,NA)
  else{
    print(c("ERROR: ",funcName, " not a recognized SR function in function selectSmsy"))
    break
  }
  ff
}