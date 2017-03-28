#' @title Creates a report with plots.
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", and "tDat"
#' @param dmObj.RData.file a saved DM object as a RData file with the objects "input","dat","result", and "tDat"
#' @param output.file Filename to give saved report.
#' @param rav.options list of the rav file options to use that do not come from the posteriors.
#' @param output.format list of output formats.  If input.type=="xRmd", any format that rmarkdown:::render allows is fine.  If input.type=="xRnw", only "pdf_document" is allowed.
#' @param output.dir Directory where to save the output files.  Defaults to working directory.
#' @param input.type xRmd for rmarkdown.  xRnw for knitr.
#' @return Nothing.  The report is written to a PDF and tex file.
writeReport <- function(dmObj=NULL, dmObj.RData.file=NULL,
                    output.file="report1",
                    rav.options=list(), 
                    output.format=c("pdf_document"),
                    output.dir=getwd(), input.type="xRnw")
{
  if (missing(dmObj) && missing(dmObj.RData.file)) {
    stop(paste("Report generation requires either a runModel() results object",
               "or an RData file name."))
  }
  
  if (!missing(dmObj.RData.file)) {
    rdnames <- load(dmObj.RData.file)
    reqnames <- c("input", "dat", "mlEst", "tDat", "bdat", "plist")
    missing <- reqnames[!(reqnames %in% rdnames)]
    if (length(missing) > 0) {
      stop(paste(paste(missing,collapse=','), "missing in RData\n"))
    }
    
    if (input.type=="xRnw") {
      if(!all(output.format == "pdf_document")) stop("If input.type is xRnw, then only pdf_document allowed as output.format.")
    }
    
    SRfunction <- input$SRfunction
    if (is.null(SRfunction)) {
      stop("SRfunction missing in RData$input")
    }
    
    includeMarineSurvival <- input$includeMarineSurvival
    if (is.null(includeMarineSurvival)) {
      stop("includeMarineSurvival missing in RData$input")
    }
    includeFlow <- input$includeFlow
    if (is.null(includeFlow)) {
      stop("includeFlow missing in RData$input")
    }
    
    dmObj <- list()
    dmObj$input <- input
    dmObj$dat <- dat
    dmObj$mlEst <- mlEst
    dmObj$tDat <- tDat
    dmObj$bdat <- bdat
    
    dmObj$jagsOut <- plist$jagsOut
    dmObj$otherDat <- plist$otherDat
    dmObj$priors <- plist$priors
    dmObj$calcInits <- function() { plist$theinits }
    
  } else {
    input <- dmObj$input
    population <- input$population
    includeMarineSurvival <- input$includeMarineSurvival
    includeFlow <- input$includeFlow
    SRfunction <- input$SRfunction
    dat <- dmObj$dat
    mlEst <- exp(dmObj$mlEst$estimate)
    tDat <- dmObj$tDat
    bdat <- dmObj$bdat
  }
  
  ## knitr will attempt to use the input directory as a work directory
  ## for its intermediate files.  This fails when the input directory
  ## is the package directory and the package is invoked from a server.
  ## Copy the xRmd/xRnw file to the output directory and use that as the
  ## input path.  knitr will now create its temporaries in that directory,
  ## where it must have write permission anyway.
  ## The Rnw file has file suffix xRmd/xRnw so that the file is not auto-built
  ## as vignette when the package is built.
  
  pkgpath <- find.package("DM")
  path=file.path(pkgpath, "doc", paste("Report1-knitr.",input.type,sep=""))
  rmdPath <- dirname(output.file)
  rmdPath <- file.path(rmdPath, paste("Report1-knitr.",input.type,sep=""))
  file.copy(path, rmdPath)
  path <- rmdPath
  for(out in output.format){
    if(input.type=="xRnw"){
      file.suffix = str_split(out,"_")[[1]][1]
      if(file.suffix == "pdf")
        knit2pdf(path,
                 output=paste(output.dir,"/", output.file, ".tex",sep=""),
                 envir=sys.nframe(), quiet=TRUE)
    }
    if(input.type=="xRmd"){
      extra = ""
      file.suffix = str_split(out,"_")[[1]][1]
      if(file.suffix == "slidy") {extra="-slidy"; file.suffix="html"}
      if(file.suffix == "ioslides") {extra="-ioslides"; file.suffix="html"}
      if(file.suffix == "beamer") {extra="-beamer"; file.suffix="pdf"}
      if(file.suffix == "word") {file.suffix="docx"}
      rmarkdown::render(path, output_format=out,
                        output_file=paste(output.file, extra,".",file.suffix,sep=""),
                        output_dir=getwd(),clean=FALSE)
    }
  }
  ok=file.remove(rmdPath)
}
