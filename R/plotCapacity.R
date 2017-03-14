#' @title Create plots of capacity posteriors
#' @description Capacity posterior distibution.  This is the posterior and prior distributions for the spawner-recruit capacity parameter.
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", "bdat" and "tDat"
#' @param xLims x-axis limits
#' @param xLab x-axis label
#' @param binSize The width of the bin if the plot type is histogram.
#' @param plotType ("histogram")
#' @return The plot is made.
plotCapacity <- function(dmObj, xLab="Capacity parameter",
                         plotType="histogram", binSize=NULL, xLims=NULL){
  postDraws <- getPostDraws(dmObj)
  priors <- dmObj$priors
  xMin <- priors$logCapPrior["lowerBound"]
  xMax <- priors$logCapPrior["upperBound"]
  if(is.null(xLims)) xLims <- c(xMin,xMax)
  if(is.null(binSize)) binSize <- (xLims[2]-xLims[1])/10
  xv <- seq(xMin,xMax, by = 0.1)
  yv <- dnorm(xv, priors$logCapPrior["mu"], priors$logCapPrior["tau"]^-0.5)
  if(plotType=="density"){
    xx <- density(postDraws$logCap,from=xMin,to=xMax)
    plot(0,0,xlim=c(xMin,xMax),ylim=c(0,max(c(xx$y,yv))),type="n",xlab=xLab,ylab="",yaxt="n",bty="n",xaxt="n")
    polygon(x=c(xx$x[1],xx$x,xx$x[length(xx$x)],xx$x[1],xx$x[1]),y=c(0,xx$y,0,0,xx$y[1]),col="gray")
    lines(xv, yv*mean(xx$y)/mean(yv), type = "l", ylim = c(0, max(yv) * 1.1),lwd=2)
  }else{
    breaks <- seq(xLims[1],xLims[2],by=binSize)
    hPlot <- hist(postDraws$logCap,breaks=breaks,xlab=xLab,ylab="",bty="n",col="gray",xlim=xLims,main="",xaxt="n",freq=FALSE,yaxt="n")
    lines(xv, yv*mean(hPlot$density)/mean(yv), type = "l", ylim = c(0, max(yv) * 1.1),lwd=2)
  }
  
  axis(side=1,at=log(10^(1:12)),labels=format(10^(1:12),big.mark=",",scientific=FALSE),las=2,lwd.ticks=3,cex.axis=0.75)
  zz <- as.vector(outer(2:9,10^(1:12)))
  axis(side=1,at=log(zz),labels=format(zz,big.mark=",",scientific=FALSE),lwd.ticks=2,las=2,cex.axis=0.75)
}


