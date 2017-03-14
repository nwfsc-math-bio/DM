#' @title Create posterior credible interval plots from multiple DM runs
#' @param runList a list of multiple run objects (dmObj).  Each run object in the list is from one call of runModel
#' @param paramList The parameters you want to compare from the model output.
#' @param cols The number of columns of figures in the plot.
#' @return The plot is made.
plotSimple <- function(runList, paramList=c("obsSD","procSD","prod","logCap"), cols=4){
  if(!is.list(runList)) runList <- list(runList)
  if(length(paramList)<=cols) cols <- length(paramList)
  n <- length(runList)
  numRows <- ceiling(length(paramList)/cols)
  par(mfrow=c(numRows,cols),mar=c(4,2,1,1),oma=c(2,12,1,1),xpd=NA)
  j <- 1
  modNames <- names(runList)
  if(is.null(modNames)) modNames <- paste("m",1:n,sep="")
  for(param in paramList){
    vals <- array(NA,dim=c(n,5))
    for(i in 1:n){
      vals[i,] <- quantile(unlist(getPostDraws(runList[[i]])[param]),prob=c(0.1,0.25,0.5,0.75,0.9))
    }
    plot(vals[,3],1:n,ylim=c(0.5,n+0.5),xlim=range(vals),pch=16,cex=1.25,ylab="",xlab=param,yaxt="n",bty="l")
    if((j %% cols)==1) axis(side=2,at=1:n,labels=modNames,las=2)
    segments(y0=1:n,x0=vals[,1],y1=1:n,x1=vals[,5])
    segments(y0=1:n,x0=vals[,2],y1=1:n,x1=vals[,4],lwd=3)
    j <- j+1
  }
}
