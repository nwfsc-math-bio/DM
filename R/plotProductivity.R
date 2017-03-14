#' @title Create plots of productivity posteriors
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", "bdat" and "tDat"
#' @param xLims x-axis limits
#' @param xLab x-axis label
#' @param binSize The width of the bin if the plot type is histogram.
#' @param plotType ("histogram")
#' @return The plot is made.
plotProductivity <- function(dmObj, xLab="Productivity parameter",
                             plotType="histogram",
                             binSize=NULL, xLims=NULL){
  postDraws <- getPostDraws(dmObj)
  priors <- dmObj$priors
  if(is.null(xLims)) xLims <- c(0,priors$prodPrior["upperBound"])
  if(is.null(binSize)) binSize <- (xLims[2]-xLims[1])/10
  xv <- seq(0, priors$prodPrior["upperBound"], by = 0.1)
  yv <- dlnorm(xv, priors$prodPrior["logMu"], priors$prodPrior["tau"]^-0.5)
  if(plotType=="density"){
    xx <- density(postDraws$prod,from=xLims[1],to=xLims[2])
    plot(0,0,xlim=xLims,ylim=c(0,max(c(xx$y,yv))),type="n",xlab=xLab,ylab="",yaxt="n",bty="n")
    polygon(x=c(xx$x[1],xx$x,xx$x[length(xx$x)],xx$x[1],xx$x[1]),y=c(0,xx$y,0,0,xx$y[1]),col="gray")
    lines(xv, yv*mean(xx$y)/mean(yv), type = "l", ylim = c(0, max(yv) * 1.1),lwd=2)
  }else if(plotType=="histogram"){
    breaks <- seq(xLims[1],xLims[2],by=binSize)
    hPlot <- hist(postDraws$prod,breaks=breaks,xlab=xLab,ylab="", bty="n", col="gray", xlim=xLims, main="", freq=FALSE, yaxt="n")
    lines(xv, yv*mean(hPlot$density)/mean(yv), type = "l", ylim = c(0, max(yv) * 1.1),lwd=2) 	
  }else{
    print("unrecognized plot type")
  }
}
