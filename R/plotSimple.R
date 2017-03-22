#' @title Create posterior credible interval plots from multiple DM runs
#' @param modelList a list of multiple run objects (dmObj) based on different models.  Each run object in the list is from one call of runModel
#' @param modNames  more detailed names of models. If the runs are named in the list, it uses those by default.
#' @param paramList The parameters you want to compare from the model output.
#' @param funcList functions that take the dmObj as their only parameter return a posterior distribution you want to compare across models.
#'                 the user can make their own functions or utilize one of the available functions (procVarAutoCorr, and SmsyFunc)
#' @param cols The number of columns of figures in the plot.
#' @param leftMargin the size of the left figure margin (passed to oma in par).
#' @param modOrder the order of the models. defaults to the the order in the list.
#' @return The plot is made.
simplePlot <- function(modelList,modNames=names(modelList)[modOrder], paramList=c("obsSD","procSD","prod","logCap"),
                       funcList=NULL,cols=4,leftMargin=12,modOrder=1:length(modelList)){
  if(!is.list(modelList)) modelList <- list(model)
  if(length(paramList)+length(funcList)<=cols) cols <- length(paramList)+length(funcList)
  n <- length(modelList)
  modelList <- modelList[modOrder]
  numRows <- ceiling((length(paramList)+length(funcList))/cols)
  valNames <- c(paramList,names(funcList))
  if(is.null(modNames)) modNames <- paste("m",1:n,sep="")
  valsList <- list()
  for(param in paramList){
    vals <- array(NA,dim=c(n,5))
    for(i in 1:n){
      vals[i,] <- quantile(unlist(getSims(modelList[[i]])[param]),prob=c(0.1,0.25,0.5,0.75,0.9))
    }
    valsList <- c(valsList,list(vals))
  }
  if(!is.null(funcList)){
    for(func in funcList){
      vals <- array(NA,dim=c(n,5))
      for(i in 1:n){
        vals[i,] <- quantile(func(modelList[[i]]),prob=c(0.1,0.25,0.5,0.75,0.9))
      }
      valsList <- c(valsList,list(vals))
    }
  }
  par(mfrow=c(numRows,cols),mar=c(4,2,1,1),oma=c(2,leftMargin,1,1),xpd=NA)
  j <- 1
  for(vals in valsList){   
    plot(vals[,3],1:n,ylim=c(0.5,n+0.5),xlim=range(vals),pch=16,cex=1.25,ylab="",xlab=valNames[j],yaxt="n",bty="l")
    if((j %% cols)==1) axis(side=2,at=1:n,labels=modNames,las=2)
    segments(y0=1:n,x0=vals[,1],y1=1:n,x1=vals[,5])
    segments(y0=1:n,x0=vals[,2],y1=1:n,x1=vals[,4],lwd=3)
    j <- j+1
  }
}

procVarAutoCorr <- function(DMobj){
  x <- getSims(DMobj)
  tmp <- x$predictedRecruits - x$recruits
  autoCorr <- function(x) cor(x[-1],x[-length(x)])
  apply(tmp,1,autoCorr)
}

SmsyFunc <- function(DMobj){
  x <- DMobj$tDat$Smsy
  x[is.na(x)] <- max(x,na.rm=T)
  x
}


