#' @title Creates a PDF report with plots.
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", and "tDat"
#' @param dmObj.RData.file a saved DM object as a RData file with the objects "input","dat","result", and "tDat"
#' @param output.file Filename to give saved report.
#' @param rav.options list of the rav file options to use that do not come from the posteriors.
#' @param output_format list of output formats.  Any that rmarkdown:::render allow are fine.
#' @return Nothing.  The report is written to a PDF and tex file.
Report1 <- function(dmObj=NULL, dmObj.RData.file=NULL, output.file="report1",
                    rav.options=list(), output_format=c("html_document","pdf_document"))
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
  ## Copy the Rmd file to the output directory and use that as the
  ## input path.  knitr will now create its temporaries in that directory,
  ## where it must have write permission anyway.
                
  pkgpath <- find.package("DM")
  path=file.path(pkgpath, "doc", "Report1-knitr.Rmd")
  rmdPath <- dirname(output.file)
  rmdPath <- file.path(rmdPath, "Report1-knitr.Rmd")
  file.copy(path, rmdPath)
  path <- rmdPath
  for(out in output_format){
    extra = ""
    file.suffix = str_split(out,"_")[[1]][1]
    if(file.suffix == "slidy") {extra="-slidy"; file.suffix="html"}
    if(file.suffix == "ioslides") {extra="-ioslides"; file.suffix="html"}
    if(file.suffix == "beamer") {extra="-beamer"; file.suffix="pdf"}
    if(file.suffix == "word") {file.suffix="docx"}
    rmarkdown::render(path, output_format=out,
                    output_file=paste(output.file, extra,".",file.suffix,sep=""),
                    output_dir=getwd())
  }
}
