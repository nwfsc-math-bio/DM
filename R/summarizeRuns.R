#' @title Summarize runs
#' @description Summarize DM runs
#' @param runList a list of multiple run objects (dmObj).  Each run object in the list is from one call of runModel
#' @return a data frame with summary statistics for each run
summarizeRuns <- function(runList){
  n <- length(runList)
  resTab <- data.frame(mod=rep(NA,n),SRfunc=NA,covariates=NA,DIC=NA,msCoefP=NA,flowCoefP=NA,obsSD=NA,procSD=NA)
  
  for(i in 1:n){
    dmObj <- runList[[i]]
    x <- getPostDraws(dmObj)
    coefs <- (input$includeMarineSurvival=="yes" | input$includeFlow=="yes")
    resTab$mod[i] <- dmObj$input$analysisType
    resTab$SRfunc[i] <- dmObj$input$SRfunction
    resTab$includeMarineSurvival[i] <- dmObj$input$includeMarineSurvival
    resTab$includeFlow[i] <- dmObj$input$includeFlow
    resTab$DIC[i] <- round(dmObj$DIC,1)
    resTab$msCoefP[i] <- ifelse(coefs,round(mean(x$msCoef>0),2),NA)
    resTab$flowCoefP[i] <- ifelse(coefs,round(mean(x$flowCoef<0),2),NA)
    resTab$obsSD[i] <- round(median(x$obsSD),3)
    resTab$procSD[i] <- round(median(x$procSD),3)
  }
  
  resTab
}


