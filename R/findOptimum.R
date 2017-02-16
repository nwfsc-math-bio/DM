########################################################################################
## The optimization functions findOptimum and fndOpt
## Because the likelihood surfaces for these problems often has a severe bananna shape 
## with a poorly defined maximum finding an optimum is often non-trivial
## If an optimum is given in the input file I use that as an initial value for nlm
## otherwise, I found that nlm often get's stuck in a local min.
## So here I've iterated between a genetic optimization algorithm and nlm
## This works more often but still at times misses the optimum
## This is adhoc and I'm sure there's a more sensible approach
########################################################################################
findOptimum <- function(dat,input,silent=FALSE){
  
  # if there is NOT opt values in the input file use Martin's silly opt procedure 
  if(is.na(input$prod)){
    if(!silent) cat(" Finding initial condition for nlm...\n")
    
    result <- fndOpt(list(input=input,dat=dat))
    # if there IS an opt value in the input file use it as an initial value for nlm
  }else{
    if(!silent) cat(" Finding ML SR estimates using nlm...\n")
    
    if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){
      result <- nlm(SSTfuncDM, p=log(c(input$prod,input$cap,input$msCoef,input$flowCoef)),dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=T, gradtol = 1e-8)
    } else {
      result <- nlm(SSTfuncDM, p=log(c(input$prod,input$cap,100,100)),dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=T, gradtol = 1e-8)
    }
  }
  result
}

# used to find the best fit when input has NA's for the SR parameter values
fndOpt <- function(x){ # Helper function for findOptimum  ;  x = list(dat,input)
  dat <- x$dat
  input <- x$input
  # create first rough guess at parameter values
  paramInits <- log(c(2,mean(dat$AEQR,na.rm=T),exp(0.5000),0.00000001))
  if(input$analysisType=="DM") SSTfunc <- SSTfuncDM
  else if(input$analysisType=="SR") SSTfunc <- SSTfuncSR
  else{
    print("ERROR: not a valid SR analysis type in findOptimum")
    break
  }
  
  result <- GenSA(paramInits,lower=rep(-100,4), upper=rep(100,4), SSTfunc, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, control=list(max.time=10,temperature=20000))
  for(i in 1:2){
    result <- nlm(SSTfunc, p=result$par, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=T, gradtol = 1e-8)
    result <- GenSA(result$estimate,lower=rep(-100,4), upper=rep(100,4), SSTfunc, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, control=list(max.time=5))
  }
  result <- nlm(SSTfunc, p=result$par, dat=dat, SRfunc=selectSR(input$SRfunction), input=input, hessian=T, gradtol = 1e-8)
  
  list(estimate=result$estimate,value=result$minimum)
}
