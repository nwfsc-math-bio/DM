#' @title Create plots of age composition
#' @description This function is called by plotResults().  Age composition data versus calendar year. The years with age composition data are represented with a stacked barplot. With age 2 on the bottom (black) and age 5 on the top (white). The numbers above each bar represent the age composition sample size.
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", "bdat" and "tDat"
#' @param proportions If proportions=TRUE, the age composition proportions are plotted.  If proportions=FALSE, the number of samples in each age are plotted.
#' @return The plot is made.
plotAgeComp <-function(dmObj, proportions=TRUE){
  # plot observed vs expected escapement.
  x <- getPostDraws(dmObj)
  input <- dmObj$input
  bdat <- dmObj$bdat
  BY <- dmObj$otherDat$broodYear
  aYrs <- 1:(bdat$lastYear+2) # can't have last year because I calculate 2 year old recruits for age comp.
  yrs <- 1:(bdat$lastYear+3)
  
  if(proportions) yMax <- 1.3 else yMax <- max(bdat$Asum)
  plot(BY[yrs],BY[yrs],xlim=range(BY[yrs]),ylim=c(0,yMax),
       xlab="Year",ylab="Age composition data",type="l",lty=3,bty="l",yaxt="n",bty="n")
  
  if(proportions){
    addBarsAt(t(as.matrix(bdat$A/bdat$Asum)),at=BY[bdat$AgeYears],width=0.75)
    text(x=BY[bdat$AgeYears],y=rep(1.2,length(bdat$AgeYears)),paste(bdat$Asum),srt=90)
    yLabs <- c(0,0.5,1)
    axis(side=2,at=yLabs,labels=paste(yLabs))
  }else{
    addBarsAt(t(as.matrix(bdat$A)),at=BY[bdat$AgeYears],width=0.75)
    axis(side=2)
  }
}

#subfunction for plotAgeComp
addBarsAt <- function(mm,at=1:dim(mm)[2],width=min(at[-1]-at[-length(at)])/1.25,
                      cols=gray((0:(dim(mm)[1]-1))/(dim(mm)[1]-1))){
  rect(xleft=at-width/2, ybottom=0, xright=at+width/2, ytop=mm[1,],col=cols[1])
  if(dim(mm)[1]>1){
    for(i in 2:dim(mm)[1]){
      mm[i,] <- mm[i,]+mm[i-1,]
      rect(xleft=at-width/2, ybottom=mm[i-1,], xright=at+width/2, ytop=mm[i,],col=cols[i])
    }
  }
}
