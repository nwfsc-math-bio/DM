#' @title Create plots of covariate coefficients
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", "bdat" and "tDat"
#' @param xLim x-axis limits
#' @param xLabs x-axis labels
#' @return The plot is made.
plotCoefs <- function(dmObj, xLim=c(-3,3),
                      xLabs=c("Marine Survival coefficient","Flow coefficient")){
  postDraws <- getPostDraws(dmObj)
  priors <- dmObj$priors
  i <- 1
  for(var in c("msCoef","flowCoef")){
    pVar <- paste(var,"Prior",sep="")
    if(is.null(xLim))xLim=c(priors[[pVar]]["lowerBound"],5)
    xx <- density(x[[var]],from=xLim[1],to=xLim[2])
    xv <- seq(xLim[1],xLim[2], by = 0.1)
    yv <- dnorm(xv, priors[[pVar]]["mu"], priors[[pVar]]["tau"]^-0.5)
    plot(0,0,xlim=xLim,ylim=c(0,max(c(xx$y,yv))),type="n",xlab=xLabs[i],ylab="",yaxt="n",bty="n")
    polygon(x=c(xx$x[1],xx$x,xx$x[length(xx$x)],xx$x[1],xx$x[1]),y=c(0,xx$y,0,0,xx$y[1]),col="gray")
    lines(xv, yv*mean(xx$y)/mean(yv), type = "l", ylim = c(0, max(yv) * 1.1),lwd=2)
    lines(c(0,0),c(0,10000),lwd=1,lty=3)
    i <- i+1
  }
}