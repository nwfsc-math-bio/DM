#' @title Create plots of priors for productivity and log capacity.
#' @description This function is used by plotResults()
#' @param priors List of priors parameters.  Created with createPriors().
#' @return The plot is made.
plotPriors <- function(priors){
  par(mfrow=c(2,1))
  xv <- seq(0,priors$prodPrior["upperBound"],by=0.1)
  yv <- dlnorm(xv,priors$prodPrior["logMu"],priors$prodPrior["tau"]^-0.5)
  plot(xv,yv,type="l",ylim=c(0,max(yv)*1.1),xlab="productivity",ylab="",yaxt="n",bty="l")
  
  xv <- seq(priors$logCapPrior["lowerBound"],priors$logCapPrior["upperBound"],by=0.1)
  yv <- dnorm(xv,priors$logCapPrior["mu"],priors$logCapPrior["tau"]^-0.5)
  plot(xv,yv,type="l",ylim=c(0,max(yv)*1.1),xlab="log capacity",ylab="",yaxt="n",bty="l")
}

