plotPosteriors=function(RData.file){
  load(RData.file)
  par(mfrow=c(2,2))
#Panel A
# plot posterior distribution for a and b with the best estimate from the optimization (on log scale)
xLim <- quantile(tDat$a,c(0.005,0.995))
yLim <- quantile(log(tDat$b),c(0.005,0.995))
plot(tDat$a,log(tDat$b),type="p", xlab="a",ylab="log(b)",xlim=xLim,ylim=yLim,col=rgb(0.5,0.5,0.5,0.25),pch=16)
points(exp(result$estimate[1]),result$estimate[2],pch=16,cex=1.5)
legend("topright","A", bty="n")

# plot uncertainty in curves
if(is.na(input$MSYfirstYear)){
  input$MSYfirstYear <- input$firstYear
  input$MSYlastYear <- input$lastYear
}
if(input$covariates=="no" & !("c" %in% names(tDat))){
  tDat$c <- rep(1,length(tDat[,1]))
  tDat$d <- rep(-1,length(tDat[,1]))
}
yrInd <- which(dat$broodYear %in% input$MSYfirstYear:input$MSYlastYear)
meanInd <- numeric(length(tDat$a))
for(i in 1:length(tDat$a)){
  meanInd[i] <- mean(exp(tDat$d[i]*dat$flow[yrInd])*dat$marineSurvivalIndex[yrInd]^tDat$c[i])
}
if(input$covariates=="no") meanInd <- rep(1,length(tDat[,1]))
notNA <- !is.na(dat$AEQR)
n <- length(tDat$a)
xx <- max(dat$totalSpawnersAge3to5[notNA])*(0:1000)/1000
plot(dat$totalSpawnersAge3to5,dat$AEQR,xlim=c(0,max(dat$totalSpawnersAge3to5[notNA],na.rm=T)),
     ylim=c(0,max(dat$AEQR,na.rm=T)*1),xlab="S",ylab="R")
samp <- sample(1:n,20,replace=F)
zeroFlow <- rep(0,length(xx))
unitMarineSurvival <- rep(1,length(xx))
for(i in samp){
  lines(xx,selectSR(input$SRfunc)(xx,zeroFlow,unitMarineSurvival,c(log(tDat$a[i]),log(tDat$b[i]),log(tDat$c[i]),log(-tDat$d[i])))*meanInd[i],col=rgb(0.5,0.5,0.5,0.2),lwd=2)
  points(tDat$Smsy[i],selectSR(input$SRfunc)(tDat$Smsy[i],0,1,c(log(tDat$a[i]),log(tDat$b[i]),log(tDat$c[i]),log(-tDat$d[i])))*meanInd[i],pch=16,cex=0.5)
  if(input$covariates=="yes"){
    points(dat$totalSpawnersAge3to5[notNA],selectSR(input$SRfunc)(dat$totalSpawnersAge3to5[notNA], dat$flow[notNA],dat$marineSurvivalIndex[notNA], c(log(tDat$a[i]), log(tDat$b[i]),log(tDat$c[i]), log(-tDat$d[i]))),col=rgb(0.5,0.5,0.5,0.2),pch=16)
  }
}
lines(c(0,10000),c(0,10000),lty=3)
lines(xx,selectSR(input$SRfunc)(xx,zeroFlow,unitMarineSurvival,result$estimate)*mean(exp(-exp(result$estimate[4])*dat$flow[yrInd])*dat$marineSurvivalIndex[yrInd]^exp(result$estimate[3])),lwd=2,lty=2)
legend("topright","B", bty="n")

#Panel C uncertainty in S as SMSY
breaks <- seq(0,ceiling(max(log(tDat$Smsy))),by=1)
hist(log(tDat$Smsy), breaks=breaks, col="gray",xaxt="n", xlab="S at MSY", ylab="", main="")
aVals <- 10^(0+(0:4)*2)
axis(side=1,at=log(aVals),labels=format(aVals, scientific=FALSE, trim=TRUE, justify="centre", big.mark=","))
box() #make box
legend("topright","C", bty="n")

#Panel D. plot posteriors for process error variables.
if(input$covariates=="yes"){
  # 4 parameter plots
  MSE <- tDat$MSE
  autoCorr <- tDat$autoCorr
  xLim <- quantile(MSE,c(0.005,0.995))
  yLim <- quantile(autoCorr,c(0.005,0.995))
  plot(MSE,autoCorr,type="p", xlab="MSE",ylab="Autocorrelation",xlim=xLim,ylim=yLim,col=rgb(0.5,0.5,0.5,0.25),pch=16)
  # point estimate
  xx <- selectSR(input$SRfunc)(dat$totalSpawnersAge3to5,dat$flow,dat$marineSurvivalIndex,result$estimate)
  yy <- ((log(dat$AEQR)-log(xx))^2)[dat$broodYear %in% (input$firstYear:input$lastYear)]
  yy <- yy[!is.na(yy)]
  nn <- length(yy)
  MSEp <- sum(yy)/(nn-4)
  autoCorrp <- cor(yy[-nn],yy[-1])
  points(MSEp,autoCorrp,pch=16,cex=1.5)
} else {
  # 2 parameter plots
  xx <- selectSR(input$SRfunc)(dat$totalSpawnersAge3to5,dat$flow,dat$marineSurvivalIndex,result$estimate)
  yy <- (dat$AEQR/xx)[dat$broodYear %in% (input$firstYear:input$lastYear)]
  mm1 <- mean(yy,na.rm=T)
  ss1 <- sd(yy,na.rm=T)
  gammaA <- mm1*mm1/(ss1*ss1)
  gammaB <- ss1*ss1/mm1
  meanSR <- tDat$meanSR
  sdSR <- tDat$sdSR
  xLim <- quantile(meanSR,c(0.005,0.995))
  yLim <- quantile(sdSR,c(0.005,0.995))
  plot(meanSR,sdSR,type="p", xlab="mean",ylab="stdev",xlim=xLim,ylim=yLim,col=rgb(0.5,0.5,0.5,0.25),pch=16)
  points(mm1,ss1,pch=16,cex=1.5)
}
legend("topright","D", bty="n")
  caption = "Panel A) Uncertainty in a and b. This plot show a sample from the joint posterior distribution of a and b. The dark circle is the best estimate from the optimization. Panel B) Uncertainty in the SR curves. The gray lines are SR curves based on samples from the joint posterior distribution (panel A). The dashed line is the SR curve corresponding to the best fit. The straight dotted line is the 1 to 1 line. The open circles are the SR data. The gray points are the predicted recruits accounting for flow and marine survival. The black points are the S at MSY for each of the SR relationships (some may not be visible since they occur further to the right). Panel C) This shows samples from the posterior distribution of S at SMSY, computed from the SR parameter posterior draws in panel A. This SMSY is used to set the upper threshold for the RER calculation."
  invisible(caption)
}
