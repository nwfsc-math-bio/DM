#' @title Helper file for creating text for the reports.
#' 
#' @description This uses data in the RData file from a saved DM run 
#' to create text describing the model fit.
#' 
#' @param RData.file a saved DM run as a RData file with the objects "input","dat","result", and "tDat"
#' 
#' @return a list with text strings.
info=function(RData.file){
  load(RData.file)
  thetext=c("<h3>Analysis of uncertainty in ", str_proper(input$SRfunction), " SR parameters and propagation to VRAP RERs for the ", input$population,"</h3>")
  
  thetext = c(thetext, "The SR model being fit is a ")
  if(input$SRfunction=="ricker") thetext=c(thetext, " Ricker function with ")
  if(input$SRfunction=="bevertonHolt") thetext=c(thetext, " Beverton-Holt function with ")
  if(input$SRfunction=="hockeyStick") thetext=c(thetext, " Hockey-stick function with ")
  if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes"){ 
    thetext=c(thetext, "four parameters a, b, c and d.  a and b are the SR function parameters and c and d incorporate the effect of marine conditions and stream flow, respectively, on recruitment.")
  }else{
    thetext=c(thetext, "two parameters a and b:</br>")
  }
  
  if(input$SRfunction=="ricker"){
    thetext=c(thetext, "R=S e^(a-S/b)")
    if(input$includeMarineSurvival=="yes") thetext=c(thetext, " exp(c log(M))")
    if(input$includeFlow=="yes") thetext=c(thetext, " exp(d log(F))")
    if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes") 
thetext=c(thetext, " where F is stream flow and M is the marine condition index")
    thetext=c(thetext, ".</br>")
  }
  if(input$SRfunction=="bevertonHolt"){
    thetext=c(thetext, "R=S/(S/b + 1/a)")
    if(input$includeMarineSurvival=="yes") thetext=c(thetext, " exp(c log(M))")
    if(input$includeFlow=="yes") thetext=c(thetext, " exp(d log(F))")
    if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes") 
      thetext=c(thetext, " where F is stream flow and M is the marine condition index")
    thetext=c(thetext, ".</br>")
  }
  if(input$SRfunction=="hockeyStick"){
    thetext=c(thetext, "R=S exp(a)")
    if(input$includeMarineSurvival=="yes") thetext=c(thetext, " exp(c log(M))")
    if(input$includeFlow=="yes") thetext=c(thetext, " exp(d log(F))")
    thetext=c(thetext, " if S a < b.</br>")
    thetext=c(thetext, "R=b")
    if(input$includeMarineSurvival=="yes") thetext=c(thetext, " exp(c log(M))")
    if(input$includeFlow=="yes") thetext=c(thetext, " exp(d log(F))")
    thetext=c(thetext, " otherwise")
    if(input$includeMarineSurvival=="yes" | input$includeFlow=="yes") 
      thetext=c(thetext, ".</br> where F is stream flow and M is the marine condition index")
    thetext=c(thetext, ".</br>")
  }
  
  thetext=c(thetext,"R is the number of recruits and S is number of spawners.  This SR function is fit to the spawner-recruit data in the A and P file using vague priors on the parameters in order to compute the uncertainty in the estimates of the SR parameters.  </br></br>The A and P tab shows the input data from the A and P file.  The Posteriors tab summarizes the SR parameter uncertainty.")
  return(thetext)
}