#' @title Create plots from DM runs
#' 
#' @description Create several plots based on the DM model runs. This calls the
#' functions for various plots: plotPriors, plotPosteriors, plotAgeComp, plotCoefs
#' 
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", and "tDat"
#' @param plotType The plots and order in which to make them
#' \describe{
#' \item{covariates}{}
#' \item{harvest}{}
#' \item{EscData}{}
#' \item{EscAndAge}{}
#' \item{escapement}{}
#' \item{SR}{}
#' \item{recruits}{recruit residuals, recruits/spawners #and effective harvest on recruits}
#' \item{barPlot}{a bar plot showing spawners and recruits broken down by pHOS and harvested}
#' \item{ageComp}{}
#' \item{priors}{Priors used in the DM model}
#' \item{a.and.p.data}{Raw data from the A and P input file.}
#' }
#' @param plotDest Plot destination. Can be "screen", "png", "default", "none". If "screen", separate graphics windows are opened. If "none", nothing is plotted but the figure captions are returned.
#' @param plotName prefix to apply to filename for generated plots if plotDest="png"
#' 
#' @return The plot is saved to a file or plotted to screen or default device depending on plotDest. A vector of plot captions is returned invisibly.
plotResults <-function(dmObj,
                       plotType=c("a.and.p.data", "priors", "SR", "recruits", "covariates", "harvest", "EscData", "EscAndAge", "escapement", "barPlot", "ageComp"),
                       plotDest="default", plotName="tmp"){
  
  # other plots to include in the future
  # - recruit residuals vs escapement
  # - recruit residuals w/ and w/o covariate effects
  
  # create some intermediate values for plotting
  x <- getPostDraws(dmObj)
  input <- dmObj$input
  bdat <- dmObj$bdat
  hr <- calculateHarvested(dmObj)
  BY <- dmObj$otherDat$broodYear
  dat <- dmObj$dat
  
  plotCaps = c()
  #specify which devices should be turned off.
  dev.off.dests = c("png")
  
  #Calculate the nn, escTmp and Sq
  # estimate of escapement. Will be a parameter in the SS model and is set equal to the observed data in the DM model
  nn <- dim(x$recruits)[2]
  if(input$analysisType=="SS"){
    escTmp <- x$wildEscapementAge3to5[,1:nn]
    for(i in 1:nn){
      escTmp[,i] <- escTmp[,i]/(1-bdat$pHOS[i]) # wild origin spawners correctied to include all "natural" "in river" spawners.
    }
    Sq <- apply(escTmp,2,quantile,prob=c(0.1,0.5,0.9))
  }else{
    escTmp <- bdat$wildEscapementAge3to5obs[1:nn]
    escTmp <- escTmp/(1-bdat$pHOS) # wild origin spawners corrected to include all "natural" "in river" spawners.
    Sq <- rbind(escTmp,escTmp,escTmp)
  }
  
  for( plt in plotType){
    # plot covariates
    if(plt == "covariates"){
      plotCaps = c(plotCaps, "The freshwater and marine survival covariates versus brood year.  The covariate appears in the spawner recruit function.")
      if(plotDest=="none") next
      if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){
        if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=7,units="in",res=400)
        if(plotDest=="screen") dev.new(width=8, height=7, noRStudioGD=TRUE)
        logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
        logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
        flow <- exp(bdat$logFlow + logFlowMu)
        MSind <- exp(bdat$logMarineSurvivalIndex + logMSMu)
        par(mfrow=c(2,1),mar=c(2,4,5,0),oma=c(3,3,1,2))
        yrInd <- 1:bdat$lastYear
        plot(BY[yrInd],flow[yrInd],xlab="",ylab="Flow var",bty="l",type="o",pch=16)
        plot(BY[yrInd],MSind[yrInd],xlab="Brood Year",ylab="Marine survival index",bty="l",type="o",pch=16)
        if(plotDest %in% dev.off.dests) dev.off()
      }
    }
    
    # plot harvest rates
    if(plt == "harvest"){
      plotCaps = c(plotCaps, "The mixed maturity fishing rate and mixed fishing rate by brood year and age. The different shaded bars represent the different age where age 2 are the darkest and age 5 are the lightest.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=7,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=7, noRStudioGD=TRUE)
      par(mfrow=c(2,1),mar=c(2,4,5,0),oma=c(3,3,1,2))
      yInd <- 1:(bdat$lastYear)
      barplot(t(bdat$matureFishingRate),beside=TRUE,names.arg=paste(BY[yInd]),las=2,main="Mature Fishing Rate")
      barplot(t(bdat$mixedMaturityFishingRate),beside=TRUE,names.arg=paste(BY[yInd]),las=2,main="Mixed Maturity Fishing Rate")
      mtext(side=1,outer=TRUE,text="Brood Year",line=1)
      mtext(side=2,outer=TRUE,text="Harvest Rate",line=1)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    # plot escapement data.
    if(plt == "EscData"){
      plotCaps = c(plotCaps, "The escapement data by brood year.  The upper solid line represents the total escapement age 3 to 5. The lower dotted line represents total natural origin spawners age 3 to 5.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=5,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=5, noRStudioGD=TRUE)
      yrs <- 1:(bdat$lastYear+3)
      wildEsc <- bdat$wildEscapementAge3to5obs[yrs]
      totEsc <- bdat$wildEscapementAge3to5obs[yrs]/(1-bdat$pHOS[yrs])
      plot(BY[yrs],totEsc,ylim=c(0,max(totEsc,na.rm=TRUE)),xlab="Brood Year",ylab="Escapement",bty="l",type="o",pch=16) 
      lines(BY[yrs],wildEsc,type="o",lty=3)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    ## panel 1 plot observed vs expected escapement and age composition.
    if(plt == "EscAndAge"){
      plotCaps = c(plotCaps, "Observed spawner data with model predictions and observed 4-year olds with model predictions. The upper panel is log natural origin escapement age 3 to 5 versus brood year. The dark line represents median estimated escapement.  The light grey lines represent the uncertainty in the estimates (80 perc credible intervals).  The red lines represent an 80 perc prediction interval which incorporates observation error.  The bottom panel is the proportion of 4 year olds in the natural origin spawners.  The dark bars represent the 80/% credible intervals for the proportion of age 4 fish within the natural origin spawners.  The lines represent the median and 80/% credible interval based on the full model.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=5,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=5, noRStudioGD=TRUE)
      par(mfrow=c(2,1),mar=c(2,4,2,0),oma=c(3,1,1,2))
      esc <- apply(x$escapement,c(2,3),mean)
      yrs <- 1:(bdat$lastYear+3)
      eQ <- apply(log(x$wildEscapementAge3to5),2,quantile,prob=c(0.1,0.5,0.9))
      dd <- dim(x$wildEscapementAge3to5)
      sdMat <- array(NA,dim=dd)
      for(i in 1:dd[2]){
        sdMat[,i] <- rnorm(dd[1],0,x$obsSD)
      }
      epQ <- apply(log(x$wildEscapementAge3to5)+sdMat,2,quantile,prob=c(0.1,0.5,0.9))
      plot(BY[yrs],log(bdat$wildEscapementAge3to5obs[yrs]),pch=16,xlab="",ylab="Escapement",bty="l",ylim=range(epQ),yaxt="n")  
      pYrs <- 6:(bdat$lastYear+3)
      lines(BY[pYrs],eQ[2,pYrs])
      lines(BY[pYrs],eQ[1,pYrs],col="gray")
      lines(BY[pYrs],eQ[3,pYrs],col="gray")
      lines(BY[pYrs],epQ[1,pYrs],col=rgb(0.8,0.2,0.2,0.5),lwd=3)
      lines(BY[pYrs],epQ[3,pYrs],col=rgb(0.8,0.2,0.2,0.5),lwd=3)
      axis(side=2,at=log(10^(0:10)),labels=paste(10^(0:10)))
      grid()
      
      ## panel 2 plot the age data
      aYrs <- 1:(bdat$lastYear+2) # can't have last year because I calculate 2 year old recruits for age comp.
      aa <- array(NA,dim=c(length(x$prod),(bdat$lastYear+2),4))
      for(yr in (bdat$firstYear+5):(bdat$lastYear+2)){
        for(a in 1:4){
          aa[,yr,a] <- x$escapement[,yr-(1+a),a]
        }
        aa[,yr,] <- aa[,yr,]/apply(aa[,yr,],1,sum)
      }
      aaQ <- apply(aa,c(2,3),quantile,prob=c(0.1,0.5,0.9),na.rm=TRUE)   
      plot(BY[aYrs],aaQ[2,aYrs,3],type="l",xlim=range(BY[yrs]),ylim=c(0,1),xlab="",ylab="Proportion 4 year olds",bty="l")
      lines(BY[aYrs],aaQ[1,aYrs,3],col="gray")
      lines(BY[aYrs],aaQ[3,aYrs,3],col="gray")
      bb <- array(NA,dim=c(3,bdat$aYears,4))
      for(i in 1:bdat$aYears){
        for(a in 1:4){
          bb[,i,a] <- qbeta(c(0.1,0.5,0.9),0.5+bdat$A[i,a],0.5+bdat$Asum[i]-bdat$A[i,a])
        }  
      }
      aYrs <- ((bdat$firstYear+5):(bdat$lastYear+2))[which((bdat$firstYear+5):(bdat$lastYear+2) %in% bdat$AgeYears)]
      segments(x0=BY[aYrs],y0=bb[1,,3],x1=BY[aYrs],y1=bb[3,,3],lwd=4,col=rgb(0,0,0,1))
      grid()
      mtext(side=1,outer=TRUE,text="Brood Year",line=1)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    
    # plot effective harvest on escapement rate pHOS.
    if(plt == "escapement"){
      plotCaps = c(plotCaps, "Patterns in effective harvest rate and porportion of hatchery origin spawners. The upper panel is the effective harvest rate on the total natural origin spawners returning in a given brood year.  The bottom panel is the proportion of hatchery origin spawners for a given brood year.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=5,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=5, noRStudioGD=TRUE)
      par(mfrow=c(2,1),mar=c(2,4,2,0),oma=c(3,1,1,2))
      
      ## panel 1 plot the effective harvest rate for escapement
      hYrs <- 1:(bdat$lastYear+3)
      hRate <- hr$harvestedEsc/(hr$harvestedEsc+hr$unharvestedEsc)
      hRateQ <- apply(hRate,2,quantile,prob=c(0.1,0.5,0.9),na.rm=TRUE)
      plot(BY[hYrs],hRateQ[2,],xlim=range(BY[hYrs]),ylim=range(hRateQ,na.rm=TRUE),xlab="Year",ylab="Effective Harvest Rate (esc)",type="l",lty=3,bty="l")
      segments(x0=BY[hYrs],y0=hRateQ[1,],x1=BY[hYrs],y1=hRateQ[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.25))  
      grid()
      
      ## panel 2 plot pHOS
      phosYrs <- 1:bdat$lastYear
      plot(BY[phosYrs],bdat$pHOS[phosYrs],xlim=range(BY[phosYrs]),ylim=c(0,1),ylab="pHOS",pch=16,type="o",bty="l")
      grid()
      mtext(side=1,outer=TRUE,text="Brood Year",line=1)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    
    # posterior distribution of productivity and capacity, and SR function
    if(plt == "SR"){
      plotCaps = c(plotCaps, "Posterior of the spawner-recruit parameters and fits to the spawner-recruit data. The left panel represents the joint posterior distribution for the productivity and capacity parameters of the spawner-recruit relationship.  The grey dots represent individual samples from the posterior distribution.  Thus darker regions represent higher probability.  The orange point is the posterior median.  The blue point is the point estimate (maximum likelihood).  The right panel is total spawners age 3 to 5 versus estimated adult equivalent recruits. The vertical green lines represent uncertainty in recruitment (80/% credible intervals) and the horizontal green line represent observation uncertainty in the spawner numbers (80/% credible interval).  The black lines represent the shift from the observed spawners to the predicted spawners.  The red lines represent the spawner-recruit function for 20 samples from the posterior distribution.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""), width=8, height=5,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=5, noRStudioGD=TRUE)
      par(mfrow=c(1,2))

            #compute the centered ML estimate
      #dmObj$mlEst$estimate this is prod, cap, msCoef, flowCoef
      mlEst.cent <- dmObj$mlEst$estimate

      cap <- exp(x$logCap)
      plot(cap,x$prod,
           xlim=quantile(cap,prob=c(0.01,0.99)),
           ylim=range(x$prod,mlEst.cent["prod"]),pch=16,col=rgb(0,0,0,0.2),
           xlab="Capacity",ylab="Productivity",bty="l",xaxt="n",yaxt="n")
      initV <- dmObj$calcInits()
      tmp <- med(as.matrix(cbind(x$prod,x$logCap)),method= "Spatial")
      points(exp(tmp$median[2]),tmp$median[1],pch=16,col="orange",cex=2)
      points(mlEst.cent["cap"], mlEst.cent["prod"], pch=16, col="blue", cex=2) 
      
      xAx <- 2^(-5:5)
      nams <- ifelse(xAx<1,paste("1/",1/xAx,sep=""),paste(xAx))
      axis(side=2)
      axis(side=1)#, at=log(10^(0:9)),labels=paste(10^(0:9)))
      
      # plot of spawner vs recruits and curves
      
      # Here recruits are just 
      if(input$analysisType=="SS"){
        Rq <- apply(hr$unharvestedRecruits+hr$harvestedRecruits,2,quantile,prob=c(0.1,0.5,0.9))
      }else{
        Rq <- dmObj$otherDat$AEQR[1:nn]
        Rq <- rbind(Rq,Rq,Rq)
      }
      obsEsc <- bdat$wildEscapementAge3to5obs[1:nn]/(1-bdat$pHOS)
      
      # this plots the observed escapement with a line to the predicted escapement
      plot(obsEsc,Rq[2,],xlim=c(0,max(Sq)),ylim=c(0,max(Rq)),xlab="Spawners",ylab="Recruits",bty="l",pch=16)
      segments(x0=Sq[2,],y0=Rq[2,],x1=obsEsc,y1=Rq[2,],lwd=1,col="black") #rgb(0.4,0.3,0.7,0.25))
      #points(obsEsc,Rq[2,],lwd=2,pch=16,col="black") #rgb(0.4,0.3,0.7,0.75))
      segments(x0=Sq[2,],y0=Rq[1,],x1=Sq[2,],y1=Rq[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.25))  
      segments(y0=Rq[2,],x0=Sq[1,],y1=Rq[2,],x1=Sq[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.25))  
      lines(c(0,1000000),c(0,1000000),lty=3,col="gray")
      tmpStr <- switch(input$SRfunction,
                       hockeyStick = "pmin(pp*S,exp(cc))",
                       ricker = "pp*S*exp(-S/exp(cc))",
                       bevertonHolt = "S/(S/exp(cc) + 1/pp)"
      )
      eval(parse(text=paste("SRfunc <- function(S,pp,cc)",tmpStr)))
      ss <- seq(0,max(Sq),by=round(max(Sq)/50))
      for(i in sample(1:dim(x$prod)[1],20)){
        lines(ss,SRfunc(ss,x$prod[i],x$logCap[i]),col=rgb(0.8,0.2,0.2,0.25),lwd=2)
      }
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    
    # recruit residuals, recruits/spawners #and effective harvest on recruits
    if(plt == "recruits"){
      plotCaps = c(plotCaps, "Patterns in recruitment by year. The upper panel represents the recruitment residuals, which are log recruits minus log predicted recruits.  The green lines represent 80/% credible intervals for the standard residuals.  The red lines represent 80/% credible intervals for the residuals where predicted recruits do not include effects of the covariates. The bottom panel is log adult equivalent recruits divided by total spawners age 3 to 5. The green bars represent 80/% credible intervals.")
      if(plotDest=="none") next
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=6,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=6, noRStudioGD=TRUE)
      par(mfrow=c(2,1),mar=c(2,4,2,0),oma=c(3,1,1,2))
      
      # plot recruitment residuals
      tmp <- log(x$recruits)-log(x$predictedRecruits)
      tQ <- apply(tmp,2,quantile,prob=c(0.1,0.5,0.9))
      plot(BY[c(1,nn)],c(0,0),ylim=max(abs(tQ))*c(-1,1),xlab="",ylab="Process Error",type="l",lty=3,bty="l")
      points(BY[1:nn],tQ[2,],pch=16)
      segments(x0=BY[1:nn],y0=tQ[1,],x1=BY[1:nn],y1=tQ[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.5))
      
      if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){
        if(input$includeMarineSurvival=="yes"){
          tmp <- tmp + (x$msCoef %o% bdat$logMarineSurvivalIndex)[,1,]
        }
        if(input$includeFlow=="yes"){
          tmp <- tmp + (x$flowCoef %o% bdat$logFlow)[,1,]      
        }
        tQ2 <- apply(tmp,2,quantile,prob=c(0.1,0.5,0.9))
        points(BY[1:nn]+0.25,tQ2[2,],pch=16)
        segments(x0=BY[1:nn]+0.25,y0=tQ2[1,],x1=BY[1:nn]+0.25,y1=tQ2[3,],lwd=3,col=rgb(0.8,0.2,0.2,0.5))
      }
      
      # plot the year specific natural productivity recruits/spawners
      RpS <- log(x$recruits) - log(escTmp)
      RpSq <- apply(RpS,2,quantile,prob=c(0.1,0.5,0.9))
      plot(c(1900,2100),c(0,0),xlim=BY[c(1,nn)],ylim=range(RpSq),xlab="",ylab="log(Recruits/Spawner)",type="l",lty=3,yaxt="n",bty="l")
      points(BY[1:nn],RpSq[2,],pch=16)
      segments(x0=BY[1:nn],y0=RpSq[1,],x1=BY[1:nn],y1=RpSq[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.5))
      xAx <- 2^(-5:5)
      nams <- ifelse(xAx<1,paste("1/",1/xAx,sep=""),paste(xAx))
      axis(side=2, at=log(xAx),labels=nams,las=2)
      
      # plot the effective harvest rate for recruits
      # hYrs <- 1:bdat$lastYear
      # hRate <- hr$harvestedRecruits/(hr$harvestedRecruits+hr$unharvestedRecruits)
      # hRateQ <- apply(hRate,2,quantile,prob=c(0.1,0.5,0.9))
      #plot(BY[hYrs],hRateQ[2,],xlim=range(BY[hYrs]),ylim=range(hRateQ),xlab="",ylab="Effective Harvest Rate (rec)",type="l",lty=3,bty="l")
      #segments(x0=BY[hYrs],y0=hRateQ[1,],x1=BY[hYrs],y1=hRateQ[3,],lwd=3,col=rgb(0.2,0.8,0.2,0.25))  
      
      mtext(side=1,outer=TRUE,text="Brood Year",line=1)  
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    # a bar plot showing spawners and recruits broken down by pHOS and harvested  
    if(plt == "barPlot"){
      plotCaps = c(plotCaps, "Observed spawner data and estimated recruitment. For each year there are two bars.  The left bar represents total escapement age 3 to 5.  The right bar represents adult equivalent recruits.  The escapement bar (left bar) is broken into the natural origin escapement in blue and hatchery origin escapement in tan.  The right bars are broken into unharvested (black) and harvested (grey) recruits.")
      if(plotDest=="none") next      
      if(plotDest=="png") png(paste(plotName, "_", plt, ".png",sep=""),width=8,height=4,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=4, noRStudioGD=TRUE)
      par(mar=c(2,4,2,0),oma=c(3,1,1,2))
      
      hRate <- hr$harvestedRecruits/(hr$harvestedRecruits+hr$unharvestedRecruits)
      hRateQ <- apply(hRate,2,quantile,prob=c(0.1,0.5,0.9))
      
      # plot the spawners and recruits with pHOS and harvest
      # calculate total BY recruits and proportion that were harvested.
      # calculate total observed escapement and % that are hatchery origin
      totRecQ <- apply(hr$harvestedRecruits+hr$unharvestedRecruits,2,quantile,prob=c(0.1,0.5,0.9))
      
      Z1 <- rep(0,nn)
      E1 <- (Sq[2,]*(1-bdat$pHOS))
      E2 <- (Sq[2,])
      R1 <- (totRecQ[2,]*(1-hRateQ[2,]))
      R2 <- (totRecQ[2,])
      
      d1 <- 0.3
      d2 <- 0.05
      plot(c(1900,2100),c(0,0),xlim=BY[c(1,nn)],ylim=range(c(0,E2,R2)),xlab="",ylab="Total Adults",type="n",lty=3,yaxt="n",bty="l")
      rect(xleft=BY[1:nn]-d1,ybottom=Z1,xright=BY[1:nn]-d2,ytop=E1,col="steel blue")
      rect(xleft=BY[1:nn]-d1,ybottom=E1,xright=BY[1:nn]-d2,ytop=E2,col="tan")  
      rect(xleft=BY[1:nn]+d1,ybottom=Z1,xright=BY[1:nn]+d2,ytop=R1,col="black")
      rect(xleft=BY[1:nn]+d1,ybottom=R1,xright=BY[1:nn]+d2,ytop=R2,col="gray")
      axis(side=2)
      mtext(side=1,outer=T,text="Brood Year",line=1)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    # plot of age comp data
    if(plt == "ageComp"){
      plotCaps = c(plotCaps, "Age composition data versus calendar year. The years with age composition data are represented with a stacked barplot. With age 2 on the bottom (black) and age 5 on the top (white). The numbers above each bar represent the age composition sample size.")
      if(plotDest=="none") next      
      if(plotDest=="png") png(paste(plotName,"ageComp.png",sep="_"),width=8,height=4,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=4, noRStudioGD=TRUE)
      plotAgeComp(dmObj)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    # plot A & P raw data
    if(plt == "a.and.p.data"){
      plotCaps = c(plotCaps, "Raw data from the A and P files. These are the input data for the DM model.")
      if(plotDest=="none") next      
      if(plotDest=="png") png(paste(plotName,"a-and-p-data.png",sep="_"), width=8, height=8, units="in", res=400)
      if(plotDest=="screen") dev.new(width=8, height=8, noRStudioGD=TRUE)
      dat <- dmObj$dat
      # plot of the original data
      par(mfrow=c(3,2))
      nadat=is.na(dat$totalSpawnersAge3to5)|dat$broodYear==0
      plot(dat$broodYear[!nadat],dat$totalSpawnersAge3to5[!nadat],type="l", xlab="",ylab="total spawners",main="Total Spawners age 3 to 5")
      nadat=is.na(dat$totalWildEscapementAge3to5)|dat$broodYear==0
      plot(dat$broodYear[!nadat],dat$totalWildEscapementAge3to5[!nadat],type="l", xlab="",ylab="total wild esc.",main="Total Wild Esc. age 3 to 5")
      nadat=is.na(dat$preSpawnMortRate[,1])|dat$broodYear==0
      matplot(dat$broodYear[!nadat],dat$preSpawnMortRate[!nadat,],type="p", xlab="",ylab="mort. rate",main="pre-spawning mort. rate")
      nadat=is.na(dat$maturationRate[,1])|dat$broodYear==0
      matplot(dat$broodYear[!nadat],dat$maturationRate[!nadat,],type="p", xlab="",ylab="mat. rate",main="maturation rate")
      nadat=is.na(dat$matureFishingRate[,1])|dat$broodYear==0
      matplot(dat$broodYear[!nadat],dat$matureFishingRate[!nadat,],type="p", xlab="",ylab="fish. rate",main="Mature Fish. Rate")
      nadat=is.na(dat$mixedMaturityFishingRate[,1])|dat$broodYear==0
      matplot(dat$broodYear[!nadat],dat$mixedMaturityFishingRate[!nadat,],type="p", xlab="",ylab="fish. rate",main="Mixed Mat. Fishing Rate")
      
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    # plot of priors
    if(plt == "priors"){
      plotCaps = c(plotCaps, "Priors used in the DM model.")
      if(plotDest=="none") next      
      if(plotDest=="png") png(paste(plotName,"priors.png",sep="_"),width=8,height=8,units="in",res=400)
      if(plotDest=="screen") dev.new(width=8, height=8, noRStudioGD=TRUE)
      plotPriors(dmObj$priors)
      if(plotDest %in% dev.off.dests) dev.off()
    }
    
    
  } #plt in plotType
  names(plotCaps) = plotType
  invisible(plotCaps)
}