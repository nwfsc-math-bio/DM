DMMain=function(a.and.p.file=NULL, RData.file=NULL, output.file=NULL, srFunc=NULL, covariates=NULL, show.file=TRUE, rav.options=list()){
  if(missing(a.and.p.file) & missing(RData.file)) inp=c("AP","RData")[menu(c("Use A and P file to create .Rdata file.","Use .RData file saved posteriors."), graphics = FALSE, title = "\nWhat data should be used as input?")]
  if(!missing(a.and.p.file) & !missing(RData.file)) stop("Either specify an A and P file or a saved .RData file.  Not both.")
  if(!missing(a.and.p.file)) inp="AP"
  if(!missing(RData.file)) inp="RData"  #if both !null, then uses RData
  if(inp=="RData"){
    if(missing(RData.file)){
      cat("\nChoose a .Rdata file of stored results\n")
      RData.file=file.choose()
    }
    a=load(RData.file)
    if(!all(c("population","input","dat","result","tDat") %in% a))
      stop("The loaded stored Rdata file is missing population, input, dat, result or tDat",call.=FALSE)
    mm <- ifelse(input$covariates=="yes",1,0)
    ff <- ifelse(input$covariates=="yes",1,0)
    if(missing(output.file)) output.file <- paste(input$population,"_", input$SRfunction,2+mm+ff,"_", Sys.Date(), sep="")
    
  }
  if(inp=="AP"){
    if(missing(srFunc)){
      srFunc=c("ricker","bevertonHolt","hockeyStick")[menu(c("Ricker","Beverton-Holt","Hockey-Stick"), graphics = FALSE, title = "\nSelect SR function:")]
    }else{
      if(!is.character(srFunc)) stop("srFunc should be ricker/bevertonHolt/hockeyStick")
      srFunc=str_sub(tolower(srFunc),1,1)
      if(!(srFunc %in% c("r","b","h"))) stop("srFunc should be ricker/bevertonHolt/hockeyStick")
      if(srFunc=="r") srFunc="ricker"
      if(srFunc=="b") srFunc="bevertonHolt"
      if(srFunc=="h") srFunc="hockeyStick"
    }
    if(missing(covariates)){
      covariates=c("no","yes")[menu(c("No","Yes"), graphics = FALSE, title = "\nInclude marine conditions and stream flow as covariates?")]
    }else{
      if(!is.character(covariates)) stop("covariates should be no/yes")
      covariates=str_sub(tolower(covariates),1,1)
      if(!(covariates %in% c("n","y"))) stop("covariates should be no/yes")
      if(covariates=="n") covariates="no"
      if(covariates=="y") covariates="yes"
    }
    if(missing(a.and.p.file)){
      cat("\nChoose the A & P input file.  Type ?readDMData for help.\n")
      a.and.p.file=file.choose()
    }
    #this will set up the input and dat lists
    tmp <- readDMData(a.and.p.file, srFunc, covariates)
    dat <- tmp$dat
    input <- tmp$input
    population=tmp$input$population
    
    #run BUGS and get the posteriors
    tmp = getPosteriors(dat, input)
    result = tmp$result
    tDat = tmp$tDat
    
    #Save the output
    mm <- ifelse(input$covariates=="yes",1,0)
    ff <- ifelse(input$covariates=="yes",1,0)
    
    if(missing(output.file)) output.file <- paste(input$population,"_", input$SRfunction,2+mm+ff,"_", Sys.Date(), sep="")
    
    #Write posteriors to .csv and RData file
    writeResultsToFile(population, input, dat, tDat, result, file=output.file)
  }
  
  if(str_sub(output.file,-4)!=".rav") rav.file <- paste(output.file, ".rav", sep="")
  
  createRAVfile( dat, input, result, tDat, filename=rav.file, options=rav.options )
}