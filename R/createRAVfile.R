# This function takes the output from the DM model and creates a RAV file that can be used for 
#   running the VRAP simulations. The parameter simInd indicates which MCMC sim to use.
# This takes the information from the input file in the DM spreadsheet (read in by readDMData)
#   and the results of the MCMC simulations. 
# Current version has no uncertainty in management error and base exploitation rate must be included
#   as a parameter. This could be calculated with the SR params, maturation rates, natMort, and harvest.
createRAVfile = function(
  dat,
  input,
  result,
  tDat,
  filename = "temp_rav.rav",
  estimateType = "Bayes", # can be "Bayes" or "MLE" (SR params)
  options=list()
) {
  tmp.options = list(
    simInd = 1, # which simulated of the sets of simulated parameter values to use (between 1 and numSims)
    numRuns = 1000,
    numYears = 25,
    minAge = 2,
    maxAge = 5,
    modelDepensation = "NO",
    depensation = 300,
    QETcritical = 63,
    depensPar3 = 1 ,
    lowerEscThreshold = 200, 
    recruitsFromAdultSpawners = "YES",
    SRvariation = "YES",
    baseExploitationRate = 0.70
    )
  # recruitsFromAdultSpawners and SRvariation are set at YES.  User will need to edit .rav file to change
  
  if(!is.list(options)) stop("options argument must be a list")
  
  if(!is.null(names(options))){
    for(i in names(options)){
      tmp.options[i]=options[i]
    }
  }
  options=tmp.options
  
  calcAutoCorr <- function(x) cor(x[-1],x[-length(x)],use="pairwise.complete.obs")
  
  # Convergence criterion (% error) for target ER
  convergeCrit <- 0.001
  
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
  
  # figure out number of parameters for SR function
  numParams <- ifelse(input$includeFlow=="yes",ifelse(input$includeMarineSurvival=="yes",4,3),2)
  
  # Norma Jean Sands:
  # FOR RapVuiability.exe version 2.2.4:
  # 
  # For Ric2 , Ric3, and Ric4 first parameter is intrnsic productivity and second parameter is capacity (# of fish ).  
  # Third parameter is that for marine survival index (not used for Ric2 or Ric3) and fourth parameter is that for 
  # freshwater survival index (not used for Ric2).
  # 
  # R = a S exp(-S/b) M^c exp(dF)
  # 
  # For Bev2, Bev3,  and Bev4, first parameter is intrinsic productivity (slope at orgin) and second parameter is max recruits.  
  # Third parameter is that for marine survival index (not used for Bev2 or Bev3) and fourth parameter is that for freshwater 
  # survival index (not used for Bev2).
  # 
  # R = [1/( (1/b) + (1/(a*S))  )]  M^c  exp(dF)
  # 
  # For Hoc2, Hoc3, and Hoc4, first parameter is intrinsic productivity and second is max recruits.  Third parameter is that for 
  # marine survival index (not used for Hoc2 or Hoc3) and fourth parameter is that for freshwater survival index (not used for Hoc2).
  # 
  # R = min(aS, b)  M^c  exp(dF) 
  
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
  if(estimateType=="Bayes"){
    # note that calcPEandSmsy converts parameters to form used in RAV files
    SRparameters <- tDat[options$simInd,c("a","b","c","d")]
  }else if(estimateType=="MLE"){
    # need to exponentiate all p's and change sign of p[4] to 
    #   convert from the findOptimum p format to the a,b,c,d RAV format
    SRparameters <- exp(result$estimate[1:numParams])
    SRparameters[4] <- -SRparamters[4]
  }else{
    stop("ERROR: unknown estimate type. Should be Bayes or MLE")
  }
  if(numParams < 4) SRparameters[(numParams+1):4] <- 0
  
  # Mean and CV  for marine survival index (M^c)  
  marineSurvMu <- mean(dat$marineSurvivalIndex,na.rm=T)
  marineSurvCV <- sd(dat$marineSurvivalIndex,na.rm=T)/marineSurvMu
  marineSurvMax <- max(dat$marineSurvivalIndex,na.rm=T)
  marineSurvType <- "Autoc" #"Cycle" #"Trend" #
  if(input$includeMarineSurvival=="yes"){
    msParams <- switch(marineSurvType,
                       Autoc = c(ifelse(marineSurvCV>0,calcAutoCorr(dat$marineSurvivalIndex),0),rep(0,2)),
                       Trend = c(0,rep(NULL,2)), # not sure of approach used by Norma. Lots of conditional formulas used in middleStep sheet.
                       Cycle = c(marineSurvMax - marineSurvMu, 30, 0) # amplitude, period, starting point
    )
  }else{
    msParams <- NULL
  }
  
  # Mean and CV  for flow (or other fw) index (exp(dF))
  flowMu <- mean(dat$flow,na.rm=T)
  flowCV <- sd(dat$flow,na.rm=T)/flowMu
  flowMax <- max(dat$flow,na.rm=T)
  flowType <- "Autoc" #"Cycle" #"Trend" #
  if(input$includeFlow=="yes"){
    fParams <- switch(flowType,
                      Autoc = c(ifelse(flowCV>0,calcAutoCorr(dat$flow),0),rep(0,2)),
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
    SRvarB <- tDat$MSE[options$simInd]
    SRvarCor <- tDat$autoCorr[options$simInd]
  }else{
    SRvarA <- (tDat$meanSR[options$simInd]/tDat$sdSR[options$simInd])^2
    SRvarB <- (tDat$sdSR[options$simInd]^2)/tDat$meanSR[options$simInd]
    SRvarCor <- 0.0
  }
  
  # base exploitation rate: this is the estimated/observed exploitation rate (for a 3 year period??? a base period???)
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
  manageErrorA <- 65.3946 # (derived from estimates from Puget Sound for all except STL and WRS)
  manageErrorB <- 0.0158
  
  
  # Upper escapement threshold (MSY);  # yrs to ave. 
  # Martin Liermann notes:
  #    Notice that the SMSY value can very large when the model is unconstrained and there is little information
  #    in the data about the capacity.
  msySpawners <- tDat$Smsy[options$simInd]
  numYearsToAvg <- 5
  
  # Norma Jean Sands: runType
  #   ER run  gives extinction rate, % of times less than lower threshold and % of times esc reaches upper threshold for a range of ERs.  
  #   Pop run, for a given ER,  gives exinction rate, % of times les than lower threshold and % of times esc reaches upper threshold for a range of equilbrium esc levels.  
  runType <- "ER" 
  
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
  bufferMin <- 0
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
  
  # years index for broodyears within the range firstYear:lastYear
  yrRange <- dat$broodYear %in% input$firstYear:input$lastYear
  
  # maturation rates
  averageMaturationRate <- apply(dat$maturationRate[yrRange,],2,mean,na.rm=T)
  
  # average mixed-maturity and mature fishery fishing rates
  averageMixedMaturityFishingRate <- apply(dat$mixedMaturityFishingRate[yrRange,],2,mean,na.rm=T)
  averageMatureFishingRate <- apply(dat$matureFishingRate[yrRange,],2,mean,na.rm=T)
  
  
  # create lines of text for RAV file based on calculated values and params from above
  ravText <- paste(
    input$population, ", Title\n",
    0,", Random seed; 0 gives random seed; numbers give fixed seed\n",
    options$numRuns,", Number of runs\n",
    options$numYears,", Number of years\n",
    options$minAge,", ", options$maxAge,", Minimum and maximum age (for now this is fixed; do not change)\n",
    convergeCrit,", Convergence criterion (% error) for target ER\n",
    "NO, Debug file flag\n",
    funcType,", Spawner Recruit function (Ric2;Ric3;Ric4; Bev2;Bev3;Bev4; Hoc2;Hoc3;Hoc4)\n",
    paste(formatC(as.numeric(SRparameters[1:numParams]),digits=8,format="f"),sep="",collapse=","),", S/R a; b parameters; c (Marine); d (Freshwater)\n",
    ifelse(input$includeMarineSurvival=="yes",paste(marineSurvMu,marineSurvCV,sep=", "),""),ifelse(input$includeMarineSurvival=="yes",", ",""),"Mean and CV  for marine survival index (M^c)\n",
    ifelse(input$includeMarineSurvival=="yes",paste(marineSurvType,", ",sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Marine Survival?,\n",
    paste(msParams,collapse=", "),ifelse(input$includeMarineSurvival=="yes",", ",""),"Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    ifelse(input$includeFlow=="yes",paste(flowMu,flowCV,sep=", "),""),ifelse(input$includeFlow=="yes",", ",""),"Mean and CV  for flow (or other fw) index (exp(dF))\n",
    ifelse(input$includeFlow=="yes",paste(flowType,", ", sep=""),""),"Trend; Cycle; or Autoc(orrelation) for Flow?,\n",
    paste(fParams,collapse=", "),ifelse(input$includeFlow=="yes",", ",""),"Trend/Cycle parameters: rate for trend- amplitude- period & starting pt for cycle; correl for autocorrelation\n",
    options$modelDepensation,", Depensation? (YES or NO)\n",
    options$depensation,", ",options$QETcritical,",",options$depensPar3,", 1) Esc. level for depensation to start 2) QET 3)% predicted return at QET (or for r/s=1 third parameter = 1)\n",
    options$recruitsFromAdultSpawners,", Determine recruits from adult spawners (not total)?\n",
    options$SRvariation,", Stock-recruit variation (YES or NO)\n",
    SRvarA,", ",SRvarB,", ",SRvarCor,", A and B parameters S/R error and error autocorrelation\n",
    "NO, Smolt to adult survival w/variation (YES or NO);  Deprecated replaced marine survival covariate.\n",
    "Beta distribution a and b parameters and autocorrelation\n",
    "0, Number of breakpoints; in escapement to trigger management action; Not used.\n",
    "1, Level to use as base regime\n",
    options$baseExploitationRate,", base exploitation rate\n",
    "YES, Include error (YES or NO) in ER management; Norma Jean Sands: If no, put zeros in cells A27 and B27\n",
    manageErrorA,", ",manageErrorB,", Gamma parameters for management error\n",
    options$lowerEscThreshold,", Lower escapement threshold\n",
    msySpawners,", ", numYearsToAvg,", Upper escapement threshold (MSY);  # yrs to ave.\n",
    runType,", Step ER (ER) or  Pop Capacity (Pop)?\n",
    bufferStepSize,", Buffer step size as percent of base ER or Pop capacity\n",
    bufferMin,", ",bufferMax,", Min & max buffer (x base for start & end)\n",
    paste(paste(initialPop,", Initial population size at Age ",sep=""),1:length(initialPop),"\n",collapse=""),
    paste(paste(naturalMort,", Age",sep=""),1:length(naturalMort),"natural mortality\n",collapse=""),
    paste(paste(averageMaturationRate,", Age",sep=""),2:(length(averageMaturationRate)+1),"average maturation rate\n",collapse=""),
    paste(paste(paste(averageMixedMaturityFishingRate,", ",averageMatureFishingRate,sep=""),", Age",sep=""),2:(length(averageMatureFishingRate)+1),"average mixed-maturity and mature fishery fishing rates\n",collapse=""),
    "endofinput, end of input indicator\n",
    sep="")
  
  # write to file
  cat(ravText,file=filename)
}
