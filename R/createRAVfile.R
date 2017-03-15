#' @title Create rav file.
#' 
#' @description 
#' This function takes the output from the DM model and creates a 
#' rav file that can be used forrunning the VRAP simulations. 
#' The parameter simInd indicates which MCMC sim to use. 
#' This takes the information from the input file in the DM 
#' spreadsheet (read in by readDMData) and the results of the 
#' MCMC simulations. Current version has no uncertainty in 
#' management error and base exploitation rate must be included 
#' as a parameter. This could be calculated with the SR params, 
#' maturation rates, natMort, and harvest.
#' 
#' @param bdat data for the bayesian specification of a DM run
#' @param input a list with the other values needed for a DM run. 
#' The following are examples naturalMort, analysisType = "DM", SRfunction, includeMarineSurvival, includeFlow
#' @param tDat a data frame of the transformed posteriors and calculated process error and 
#' Smsy for each draw.  From calculatePEandSmsy().  This is specifically for VRAP input.  The
#' productivity (a) and capacity (b) posteriors are not adjusted (centered) if the covariates
#' do not have 0 mean.  VRAP needs the SR parameters in this raw form using non-centered (de-meaned) covariates.
#' @param dat data from the A & P file
#' @param filename name to give the rav file
#' @param estType ("median")
#' @param sim indicates which MCMC sim from the posteriors to use if estType is not median
#' @param rav.options list of the rav file options to use that do not come from the posteriors.
#' \describe{
#'   \item{randomSeed}{(0)}
#'   \item{numRuns}{(1000) The number of simulations to run for computing 
#'   probabilities in VRAP.}
#'   \item{numYears}{(25) The number of years to project forward in the 
#'   simulations.}
#'   \item{minAge}{(2)}
#'   \item{maxAge}{(5)}
#'   \item{convergeCrit}{(0.001)}
#'   \item{centerCov}{("NO") "NO"/"YES"}
#'   \item{marineSurvType}{("Autoc") "Autoc"/"Cycle"/"Trend"}
#'   \item{flowType}{("Autoc") "Autoc"/"Cycle"/"Trend"}
#'   \item{modelDepensation}{("NO") "NO"/"YES"}
#'   \item{depensation}{(300)}
#'   \item{QETcritical}{(63)}
#'   \item{depensPar3}{(1)}
#'   \item{recruitsFromAdultSpawners}{("YES") "NO"/"YES"}
#'   \item{SRvariation}{("YES") "NO"/"YES"}
#'   \item{smoltToAdultVar}{("NO") "NO"/"YES"}
#'   \item{baseExploitationRate}{(0.67)}
#'   \item{includeManagementError}{("YES") "NO"/"YES"}
#'   \item{manageErrorA}{(65.3946) derived from estimates from Puget Sound for all except STL and WRS}
#'   \item{manageErrorB}{(0.0158) derived from estimates from Puget Sound for all except STL and WRS}
#'   \item{lowerEscThreshold}{(200)}
#'   \item{numYearsToAvg}{(5)}
#'   \item{runType}{("ER") "ER"/"Pop"}
#'   \item{bufferMin}{(0)}
#' }     
#' @return nothing is returned but the rav file is written.
createRAVfile = function(
  bdat,
  input,
  tDat,
  dat,
  filename = "temp_rav.rav",
  estType = "median",
  sim = 1,
  rav.options=list()
) {
  tmp.options = list(
    randomSeed = 0,
    numRuns = 1000,
    numYears = 25,
    minAge = 2,
    maxAge = 5,
    convergeCrit = 0.001,
    debugFlag = "NO",
    marineSurvType = "Autoc", #"Cycle" #"Trend" #
    flowType = "Autoc", #"Cycle" #"Trend" #
    modelDepensation = "NO",
    depensation = 300,
    QETcritical = 63,
    depensPar3 = 1,
    recruitsFromAdultSpawners = "YES",
    SRvariation = "YES",
    smoltToAdultVar = "NO",
    baseExploitationRate = 0.67,
    includeManagementError ="YES",
    manageErrorA = 65.3946, # (derived from estimates from Puget Sound for all except STL and WRS)
    manageErrorB = 0.0158,
    lowerEscThreshold = 200, 
    numYearsToAvg = 5,
    runType = "ER",
    bufferMin = 0 
    )
  
  # check to make sure options list is OK
  if(!is.list(rav.options)) stop("rav.options argument must be a list")
  if(!all(names(rav.options) %in% names(tmp.options))) stop("incorrect rav.option names")
  
  # replace default options with value from options list
  if(!is.null(names(options))){
    for(nam in names(options)){
      tmp.options[nam]=options[nam]
    }
  }
  options=tmp.options
  
  # function to calculate autocorrelation 
  calcAutoCorr <- function(x) cor(x[-1],x[-length(x)],use="pairwise.complete.obs")
  
  # modelDepensation (currently a function parameter)
  # Norma Jean Sands:
  #   If depensation is used, it will start at the spawner level indicated in cell A17 and is determined according to 
  #   the comment in the cell below.  Whether depensation is used or not, the three values need to be filled in, in the line below.  
  #   If depensation is not used, only the second value, QET, will be used (is used to determine extinction level of population).
  
  # Depensation parameters
  # 1) Esc. level for depensation to start 2) QET 3)% predicted return at QET (or for r/s=1 third parameter = 1) 
  #    Currently function parameters. (depensation, QETcritical, depensPar3)
  # Norma Jean Sands:
  #   For lack of better data, use mininimum population esc that showed positive return for first parameter, half of that 
  #   for QET and let third parameter = 1.
  #   When the third parameter = 1 the recruits per spawner is set at 1:1 at QETlevel of spawners.  
  #   If the third parameter = 0 then there is no return from that level of spawners (or fewer spawners).
  
  # recruits from all spawners or adult spawners  
  #    Currently a function parameter. recruitsFromAdultSpawners
  # Norma Jean Sands:
  #   Recruits may be calculated using eith all (total) spawners or, the more usual, adult spawners only.
    
  # Norma Jean Sands:
  # FOR RapVuiability.exe version 2.2.4:
  # 
  # For Ric2 , Ric3, and Ric4 first parameter is intrnsic productivity and second parameter is capacity (# of fish ).  
  # Third parameter is that for marine survival index (not used for Ric2 or Ric3) and fourth parameter is that for 
  # freshwater survival index (not used for Ric2).
  # 
  # DM speced in writeBUGScode line 41
  # R = prod*S*exp(-S/exp(logCap))*M^msCoef  exp(-flowCoef*logFlow)
  # R = prod*S*exp(-S/exp(logCap))* exp(msCoef*logMS) * exp(-flowCoef*logFlow)
  # VRAP speced in CompRecruits line 31 in VRAP package
  # Note this is not the way that Norma writes the SR funs in her documentation, but this is how it is in CompRecruits.R
  # R = a S exp(-S/b) M^c exp(dF), where F=logFlow
  # equivalent to R = a S exp(-S/b) exp(c*logMS) exp(d*logFlow)
  # a=prod; b=exp(logCap); c=msCoef, d=-flowCoef

  # For Bev2, Bev3,  and Bev4, first parameter is intrinsic productivity (slope at orgin) and second parameter is max recruits.  
  # Third parameter is that for marine survival index (not used for Bev2 or Bev3) and fourth parameter is that for freshwater 
  # survival index (not used for Bev2).
  # 
  # DM speced in writeBUGScode.R lines 39-45
  # DM R = [S/( (S/exp(logCap)) + (1/prod)  )]  M^msCoef  exp(-flowCoef*logFlow)
  # VRAP speced in CompRecruits in VRAP package
  # Note this is not the way that Norma writes the SR funs in her documentation, but this is how it is in CompRecruits.R
  # VRAP R = [S/( S/b + 1/a )] * M^c *  exp(d F), where F=logFlow 
  # a=prod; b=exp(logCap); c=msCoef, d=-flowCoef
  
  # Hoc2, Hoc3, and Hoc4, first parameter is intrinsic productivity and second is max recruits.  Third parameter is that for 
  # marine survival index (not used for Hoc2 or Hoc3) and fourth parameter is that for freshwater survival index (not used for Hoc2).
  # 
  # DM speced in writeBUGScode line 40
  # R = min(prod*S,exp(logCap))
  # VRAP speced in CompRecruits in VRAP package
  # Note this is not the way that Norma writes the SR funs in her documentation, but this is how it is in CompRecruits.R
  # R = min(aS, b)  M^c  exp(dF) 
  # a=prod; b=exp(logCap); c=msCoef, d=-flowCoef
  
    # figure out number of parameters for SR function
  if(input$includeFlow=="no" & input$includeMarineSurvival=="yes")
    stop("VRAP does not allow marine survival to be included as a covariate without flow also being included.")
  numParams <- ifelse(input$includeFlow=="yes",ifelse(input$includeMarineSurvival=="yes",4,3),2)

  # create abbreviated SR function name with number of params
  funcType <- paste(
    switch(input$SRfunction,
           hockeyStick = "Hoc",
           ricker = "Ric",
           bevertonHolt = "Bev"
    ),
    numParams,
    sep="")
    
  # SR parameters
  if(estType=="median"){
    SRparameters <- med(as.matrix(tDat[,c("a","b","c","d")]),method="Tukey")$median
  }else{
  	SRparameters <- tDat[sim,c("a","b","c","d")]
  }
  if(numParams < 4) SRparameters[(numParams+1):4] <- 0
    
  # values used to center covariates (used here to un-center values if necessary)
  logFlowMu <- mean(log(dat$flow[dat$broodYear %in% input$firstYear:input$lastYear]))
  logMSMu <- mean(log(dat$marineSurvivalIndex[dat$broodYear %in% input$firstYear:input$lastYear]))
  
  # Mean and CV  for marine survival index (M^c)  
  msInd <- if(input$centerMS) exp(bdat$logMarineSurvivalIndex+logMSMu) else exp(bdat$logMarineSurvivalIndex)
  marineSurvMu <- mean(msInd,na.rm=TRUE)
  marineSurvCV <- sd(msInd,na.rm=TRUE)/marineSurvMu
  marineSurvMax <- max(msInd,na.rm=TRUE)
  if(input$includeMarineSurvival=="yes"){
    msParams <- switch(options$marineSurvType,
                       Autoc = c(ifelse(marineSurvCV>0,calcAutoCorr(msInd),0),rep(0,2)),
                       Trend = c(0,rep(NULL,2)), # not sure of approach used by Norma. Lots of conditional formulas used in middleStep sheet.
                       Cycle = c(marineSurvMax - marineSurvMu, 30, 0) # amplitude, period, starting point
    )
  }else{
    msParams <- NULL
  }
  
  # Mean and CV  for flow (or other fw) index (exp(dF))
  flowInd <- if(input$centerFlow) bdat$logFlow+logFlowMu else bdat$logFlow
  flowMu <- mean(flowInd,na.rm=TRUE)
  flowCV <- sd(flowInd,na.rm=TRUE)/flowMu
  flowMax <- max(flowInd,na.rm=TRUE)
  if(input$includeFlow=="yes"){
    fParams <- switch(options$flowType,
                      Autoc = c(ifelse(flowCV>0,calcAutoCorr(flowInd),0),rep(0,2)),
                      Trend = c(0,rep(NULL,2)), # not sure of approach used by Norma. Lots of conditional formulas used in middleStep sheet.
                      Cycle = c(flowMax - flowMu, 30, 0) # amplitude, period, starting point
    )
  }else{
    fParams <- NULL
  }
  
  # SR variation parameters. If SRvariation = "YES" (currently a function parameter) then,
  #     either A and B parameters, OR S/R error and error autocorrelation. See below
  # Norma Jean Sands:
  #     If yes (i.e. SRvariation), for Hoc2, Ric2, Bev2 A20 and B20 are gamma pameters for variation around fit of R and C20 should be zero (is not used);  
  #     for Ric3, Hoc3, Bev3, Ric4, Hoc4, and Bev4, A20 = 0 (not used)  B20 is MSE (lognormal) around R and C20 is autocorrelation in error.
  # Martin Liermann: 
  #     These values are generated as part of the BUGs model post processing (saved in tDat)
  if(numParams>2){
    SRvarA <- 0.0
    if(estType=="median"){
      tmp <- med(as.matrix(tDat[,c("MSE","autoCorr")]),method="Spatial")$median
    }else{
      tmp <- tDat[sim,c("MSE","autoCorr")]
    }
    SRvarB <- tmp[1]
    SRvarCor <- tmp[2]
  }else{
  	if(estType=="median"){
      tmp <- med(as.matrix(tDat[,c("meanSR","sdSR")]),method="Spatial")$median
    }else{
      tmp <- tDat[sim,c("meanSR","sdSR")]
    }
    mu <- tmp[1]
    s2 <- tmp[2]
    SRvarA <- mu^2/s2 #(tDat$meanSR[options$simInd]/tDat$sdSR[options$simInd])^2
    SRvarB <- s2/mu #(tDat$sdSR[options$simInd]^2)/tDat$meanSR[options$simInd]
    SRvarCor <- 0.0
  }
  
  # base exploitation rate: this is the estimated/observed exploitation rate (for a 3 year period)
  # it looks like it is just (estimated catch) / (estimated escapement + estimated catch) (using SR params, maturation rates, and sources of mortality) 
  # Norma Jean Sands comment:
  #   For the ER run (see line 29) this ER value is just used as base for determining steps in determining range of ER to analyze (see lines 30 and 31)
  #   For the Pop run (see line 29) this is the  ER used (it may have variability around it (see line 26)) 
  # baseExploitationRate <- For now I'm reading this in as a parameters
  
  # Management error
  # Martin Liermann notes:
  #   This can vary as well. Not sure where to get
  # Norma Jean Sands:
  #   The 65.395 and 0.016 values come from a study on achieved ER compared to target ER for Puget Sound chinook populations (from CTC/comanagers)
  
  
  # Upper escapement threshold (MSY);  # yrs to ave. 
  # Martin Liermann notes:
  #    Notice that the SMSY value can very large when the model is unconstrained and there is little information
  #    in the data about the capacity.
  if(estType=="median"){
    msySpawners <- median(tDat$Smsy)
  }else{
    msySpawners <- tDat$Smsy[sim]
  }
  
  # Norma Jean Sands: runType
  #   ER run  gives extinction rate, % of times less than lower threshold and % of times esc reaches upper threshold for a range of ERs.  
  #   Pop run, for a given ER,  gives exinction rate, % of times les than lower threshold and % of times esc reaches upper threshold for a range of equilbrium esc levels.  
  # runType <- "ER" 
  
  # Buffer step size as percent of base ER or Pop capacity 
  # Norma Jean Sands:
  #   For ER selection above, this number times the base ER (line 25) gives the size of steps in ER to use when making 
  #   the range of ER values to test.
  #   For Pop selection above, this number time the base spawner recruit b parameter (cell b9) gives the size of steps 
  #   in # of fish to use when making the range of b parameter values to test.
  bufferStepSize <- 0.02/options$baseExploitationRate
  
  # Min & max buffer (x base for start & end)
  # Norma Jean Sands:
  #    For the ER selection, these two numbers times the base ER (line 25) give the start and end values of the range of ER values to test.
  #    For the Pop selection, these two numbers times the base b parameter value (line 25) give the start and end values of the range of b values to test.
  # bufferMin <- 0
  bufferMax <- 0.8/options$baseExploitationRate
  
  # Initial population size at Age 1:5 of start year (prior to nat mort) 
  # Martin Liermann notes:
  #   for now this is an parameter past with the input list since it is included in the dynamic input sheet
  #   It appears to be calculated as the average of the last few(3?) years (that can be recontructed)
  #   It is based on the run reconstruction in table 4c (i.e. start with age 5 and remove the different sources of mortality 
  #    natural mortality, prespawn mortality, mixed and terminal harvests, and move back through the ages)
  # Norma Jean Sands:
  #    These 5 lines give the starting population size for the simulations.  
  #    Default values represent where the population is currently, but the values may be changed 
  #    to see the difference in starting from a healthy population level or a depressed level.
  initialPop <- input$initialPopSize
  
  # natural mortality
  # Norma Jean Sands:
  #    The natural mortalities should remain fixed as these values were also used in the determination 
  #    of fishing rates and exploitation rates used in the run reconstruction.  
  #    If different values are used in determining fishing rate estimates, those values should be used here. 
  naturalMort <- input$naturalMort
    
  # maturation rates
  if(input$analysisType=="DM"){
    averageMaturationRate <- apply(bdat$maturationRate,2,mean,na.rm=TRUE)
  }else if(input$analysisType=="SS"){ # use multivariate median to summarize the posterior
  	if(estType=="median"){
  	  if(var(tDat$mat4)<0.001){  
        averageMaturationRate <- c(med(as.matrix(tDat[,c("mat1","mat2","mat3")]),method="Tukey",mustdith=TRUE)$median,1)
      }else{
        averageMaturationRate <- med(as.matrix(tDat[,c("mat1","mat2","mat3","mat4")]),method="Tukey",mustdith=TRUE)$median
      }
    }else{
      averageMaturationRate <- tDat[sim,c("mat1","mat2","mat3","mat4")]
    }
  }else{
    stop(paste("Unrecognized analysis Type:",input$analysisType, ". Must be SS or DM."))
  }
  
  # average mixed-maturity and mature fishery fishing rates
  averageMixedMaturityFishingRate <- apply(bdat$mixedMaturityFishingRate,2,mean,na.rm=TRUE)
  averageMatureFishingRate <- apply(bdat$matureFishingRate,2,mean,na.rm=TRUE)
  
  
  # create lines of text for RAV file based on calculated values and params from above
  ravText <- paste(
    input$population, ", Title\n",
    options$randomSeed,", Random seed; 0 gives random seed; numbers give fixed seed\n",
    options$numRuns,", Number of runs\n",
    options$numYears,", Number of years\n",
    options$minAge,", ", options$maxAge,", Minimum and maximum age (for now this is fixed; do not change)\n",
    options$convergeCrit,", Convergence criterion (% error) for target ER\n",
    options$debugFlag,", Debug file flag\n",
    funcType,", Spawner Recruit function (Ric2;Ric3;Ric4; Bev2;Bev3;Bev4; Hoc2;Hoc3;Hoc4)\n",
    paste(formatC(as.numeric(SRparameters[1:numParams]),digits=8,format="f"),sep="",collapse=","),", S/R a; b parameters; c (Marine); d (Freshwater)\n",
    ifelse(input$includeMarineSurvival=="yes",paste(marineSurvMu,marineSurvCV,sep=", "),""),ifelse(input$includeMarineSurvival=="yes",", ",""),"Mean and CV  for marine survival index (M^c)\n",
    ifelse(input$includeMarineSurvival=="yes",paste(options$marineSurvType,", ",sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Marine Survival?\n",
    paste(msParams,collapse=", "),ifelse(input$includeMarineSurvival=="yes",", ",""),"Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    ifelse(input$includeFlow=="yes",paste(flowMu,flowCV,sep=", "),""),ifelse(input$includeFlow=="yes",", ",""),"Mean and CV  for flow (or other fw) index (exp(dF))\n",
    ifelse(input$includeFlow=="yes",paste(options$flowType,", ", sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Flow?\n",
    paste(fParams,collapse=", "),ifelse(input$includeFlow=="yes",", ",""),"Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    options$modelDepensation,", Depensation? (YES or NO)\n",
    options$depensation,", ",options$QETcritical,",",options$depensPar3,", 1) Esc. level for depensation to start 2) QET 3)% predicted return at QET (or for r/s=1 third parameter = 1)\n",
    options$recruitsFromAdultSpawners,", Determine recruits from adult spawners (not total)?\n",
    options$SRvariation,", Stock-recruit variation (YES or NO)\n",
    SRvarA,", ",SRvarB,", ",SRvarCor,", A and B parameters S/R error and error autocorrelation\n",
    options$smoltToAdultVar,", Smolt to adult survival w/variation (YES or NO);  if Yes beta variation on cohort size (2 parameters) on next line\n",
    "Beta distribution a and b parameters and autocorrelation\n",
    "0, Number of breakpoints; in escapement to trigger management action\n",
    "1, Level to use as base regime\n",
    options$baseExploitationRate,", base exploitation rate\n",
    options$includeManagementError,", Include error (YES or NO) in ER management; Norma Jean Sands: If no put zeros in cells A27 and B27\n",
    options$manageErrorA,", ",options$manageErrorB,", Gamma parameters for management error\n",
    options$lowerEscThreshold,", Lower escapement threshold\n",
    msySpawners,", ", options$numYearsToAvg,", Upper escapement threshold (MSY);  # yrs to ave.\n",
    options$runType,", Step ER (ER) or  Pop Capacity (Pop)?\n",
    bufferStepSize,", Buffer step size as percent of base ER or Pop capacity\n",
    options$bufferMin,", ",bufferMax,", Min & max buffer (x base for start & end)\n",
    paste(paste(initialPop,", Initial population size at Age ",sep=""),1:length(initialPop),"\n",collapse=""),
    paste(paste(naturalMort,", Age",sep=""),1:length(naturalMort),"natural mortality\n",collapse=""),
    paste(paste(averageMaturationRate,", Age",sep=""),2:(length(averageMaturationRate)+1),"average maturation rate\n",collapse=""),
    paste(paste(paste(averageMixedMaturityFishingRate,", ",averageMatureFishingRate,sep=""),", Age",sep=""),2:(length(averageMatureFishingRate)+1),"average mixed-maturity and mature fishery fishing rates\n",collapse=""),
    "endofinput, end of input indicator\n",
    sep="")
  
  # write to file
  cat(ravText,file=filename)
}
