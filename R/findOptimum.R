#' @title Optimization functions.
#' 
#' @description Because the likelihood surfaces for these problems often has 
#' a severe bananna shape with a poorly defined maximum finding 
#' an optimum is often non-trivial If an optimum is given in the 
#' input file I use that as an initial value for nlm otherwise, 
#' I found that nlm often gets stuck in a local min. So here I 
#' have iterated between a genetic optimization algorithm and nlm.  
#' This works more often but still at times misses the optimum. 
#' This is adhoc.
#' 
#' The covariates are log-transformed and centered if input$centerMS=TRUE or input$centerFlow=TRUE.
#' 
#' @param dat data from the A & P file
#' @param input a list with the other values needed for a DM run. 
#' @param silent (TRUE/FALSE)
#' 
#' @return A list.  $estimate parameters at the minimum sum of squared residuals.  The parameters are prod, cap, msCoef, flowCoef.  $value is the sum of squared residuals at the minimum.
#' @details In SRFunctions(), bev-holt is defined as
#' S/(S*exp(-p[2])+exp(-p[1]))*exp(p[3]*logMS)*exp(p[4]*logFlow)
#' In DM (writeBUGSmodel.R), 
#' R = [S/( (S/exp(logCap)) + (1/prod)  )]  exp(marineInd*logMS)  exp(flowCoef*logFlow)
#' so
#' p[1] = log(prod), constrained to be positive
#' p[2] = log(cap), constrained to be positive
#' p[3] = msCoef,
#' p[4] = flowCoef

findOptimum <- function(dat, input, silent=FALSE){
  
  # if there is NOT opt values in the input file use Martin's fndOpt to adhoc find initial values
  if(is.na(input$prod)){
    if(!silent) cat("Finding ML SR estimates using GenSA/nlm...\n")
    result <- fndOpt(list(input=input, dat=dat))
  }else{
    if(!silent) cat("Finding ML SR estimates using nlm...\n")
    
    #set up the init for the minimization function; length of p must match the number of parameters
    paramInits = log(c(input$prod, input$cap))
    if(input$includeMarineSurvival=="yes") paramInits=c(paramInits, input$msCoef)
    if(input$includeFlow=="yes") paramInits=c(paramInits, input$flowCoef)
    
    #SSTfuncDM constructs the covariate list based on what covariates are in the model
    result <- nlm(SSTfuncDM, p=paramInits, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=TRUE, gradtol = 1e-8)
  }
  
  #Always return 4 p, but the nlm and GenSA only take the actual number of p estimates
  returnEst=c(prod=exp(result$estimate[1]), cap=exp(result$estimate[2]), msCoef=0, flowCoef=0)
  if(input$includeMarineSurvival=="yes"){ 
    returnEst[3]=result$estimate[3]
    if(input$includeFlow=="yes") returnEst[4]=result$estimate[4]
  }else{
    if(input$includeFlow=="yes") returnEst[4]=result$estimate[3]
  }
  
  list(estimate=returnEst,value=result$minimum)
  
}

# Helper function for findOptimum  ;  x = list(dat,input)
fndOpt <- function(x){ 
  dat <- x$dat
  input <- x$input
  # create first rough guess at parameter values
  #set up the init for the minimization function; length of p must match the number of parameters
  paramInits <- log(c(2,mean(dat$AEQR,na.rm=TRUE)))
  #starting values for coefficients is 0
  if(input$includeMarineSurvival=="yes") paramInits=c(paramInits, 0)
  if(input$includeFlow=="yes") paramInits=c(paramInits, 0)
  
  if(input$analysisType %in% c("DM","SS")){
    SSTfunc <- SSTfuncDM
  }else{
    print("ERROR: not a valid SR analysis type in findOptimum")
    break
  }
  
  result <- GenSA(paramInits,lower=rep(-100,4), upper=rep(100,4), SSTfunc, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, control=list(max.time=10,temperature=20000))
  for(i in 1:2){
    result <- nlm(SSTfunc, p=result$par, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=TRUE, gradtol = 1e-8)
    result <- GenSA(result$estimate,lower=rep(-100,4), upper=rep(100,4), SSTfunc, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, control=list(max.time=5))
  }
  result <- nlm(SSTfunc, p=result$par, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=TRUE, gradtol = 1e-8)
  
  result
}
