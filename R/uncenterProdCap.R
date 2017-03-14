#' @title Uncenter productivty and log capacity parameters
#' @description Transform capacity and productivity estimates to what they would be if the covariates were centered during estimation (Bayesian or ML).  VRAP uses the uncentered parameters. Note, if the coefficients were centered during estimation, input$centerMS=TRUE and/or input$centerFlow=TRUE.  These values are used to determine whether to apply the correction.  Thus input is assumed to match the parameter values in params.
#' @param dat data from the A & P file
#' @param params a vector of parameters with names flowCoef, msCoef, prod, cap.  Assumed to be from the runModel() Bayesian analysis or findOptimum() call, so centered (or not) based on input$centerFlow and input$centerMS
#' @return a vector of uncentered parameters with names flowCoef, msCoef, prod, cap.  
#' @seealso centerProdCap
uncenterProdCap <- function(input, dat, params){
  if(is.list(params))
    params=data.frame(prod=params[["prod"]], cap=exp(params[["logCap"]]),
                      msCoef=params[["msCoef"]], flowCoef=params[["flowCoef"]])
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  flowCorrect <- if(input$centerFlow & input$includeFlow=="yes") exp(-params["flowCoef"]*logFlowMu) else 1
  msCorrect <- if(input$centerMS & input$includeMarineSurvival=="yes") exp(-params["msCoef"]*logMSMu) else 1
  params["prod"] <- params["prod"] * flowCorrect * msCorrect
  if(input$SRfunction %in% c("bevertonHolt","hockeyStick")){
    params["cap"]   <- params["cap"] * flowCorrect * msCorrect
  }
  params
}

#' @title Center productivty and log capacity parameters
#' @description Center capacity and productivity so that parameters can be compared to estimates from a Bayesian analysis where the covariates were centered.
#' @param dat data from the A & P file
#' @param params a vector of parameters with names flowCoef, msCoef, prod, cap.  Assumed to be uncentered, e.g. the ML estimates from the SR model with covariates.
#' @return a vector of centered parameters with names flowCoef, msCoef, prod, cap.  
centerProdCap <- function(input, dat, params){
  if(is.list(params))
    params=data.frame(prod=params[["prod"]], cap=exp(params[["logCap"]]),
                      msCoef=params[["msCoef"]], flowCoef=params[["flowCoef"]])
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  flowCorrect <- if(input$includeFlow=="yes") exp(-1*params["flowCoef"]*logFlowMu) else 1
  msCorrect <- if(input$includeMarineSurvival=="yes") exp(-1*params["msCoef"]*logMSMu) else 1
  params["prod"] <- params["prod"] / (flowCorrect * msCorrect)
  if(input$SRfunction %in% c("bevertonHolt","hockeyStick")){
    params["cap"] <- params["cap"] / (flowCorrect * msCorrect)
  }
  params
}

#' @title Center productivty and log capacity parameters conditioned on values in input
#' @description Center capacity and productivity so that parameters can be compared to estimates from an analysis where the covariates were (possibly) centered.  VRAP uses the uncentered parameters. The input$centerFlow and input$centerMS variables specify whether and which covariates were centered for the Bayesian analysis.
#' @param dat data from the A & P file
#' @param params a vector of parameters with names flowCoef, msCoef, prod, cap.  Assumed to be uncentered, e.g. the ML estimates from the SR model with covariates.
#' @return a vector of centered parameters with names flowCoef, msCoef, prod, cap.  
conditionalcenterProdCap <- function(input, dat, params){
  if(is.list(params))
    params=data.frame(prod=params[["prod"]], cap=exp(params[["logCap"]]),
                      msCoef=params[["msCoef"]], flowCoef=params[["flowCoef"]])
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  flowCorrect <- if(input$centerFlow & input$includeFlow=="yes") exp(-params["flowCoef"]*logFlowMu) else 1
  msCorrect <- if(input$centerMS & input$includeMarineSurvival=="yes") exp(-params["msCoef"]*logMSMu) else 1
  params["prod"] <- params["prod"] / (flowCorrect * msCorrect)
  if(input$SRfunction %in% c("bevertonHolt","hockeyStick")){
    params["cap"] <- params["cap"] / (flowCorrect * msCorrect)
  }
  params
}

