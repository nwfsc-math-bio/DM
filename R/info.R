info=function(RData.file){
  load(RData.file)
  thetext=c("<h3>Analysis of uncertainty in ", str_proper(input$SRfunction), " SR parameters and propagation to VRAP RERs for the ", population,"</h3>")
  
  thetext = c(thetext, "The SR model being fit is a ")
  if(input$SRfunction=="ricker") thetext=c(thetext, " Ricker function with ")
  if(input$SRfunction=="bevertonHolt") thetext=c(thetext, " Beverton-Holt function with ")
  if(input$SRfunction=="hockeyStick") thetext=c(thetext, " Hockey-stick function with ")
  if(input$covariates=="yes"){ 
    thetext=c(thetext, "four parameters a, b, c and d.  a and b are the SR function parameters and c and d incorporate the effect of marine conditions and stream flow, respectively, on recruitment.")
  }else{
    thetext=c(thetext, "two parameters a and b:</br>")
  }
  
  if(input$SRfunction=="ricker"){
    if(input$covariates=="yes"){ 
      thetext=c(thetext, "R=S m^exp(c) e^(a - S/b - f exp(d)) </br> where f is stream flow and m is the marine condition index.</br>")
    }else{
      thetext=c(thetext, "R=S e^(a-S/b).</br>")
    }
  }
  if(input$SRfunction=="bevertonHolt"){
    if(input$covariates=="yes"){ 
      thetext=c(thetext, "R=S/(S exp(-b) + exp(-a)) exp(-f exp(d)) m^exp(c) </br> where f is stream flow and m is the marine condition index.</br>")
    }else{
      thetext=c(thetext, "R=S/(S exp(-b) + exp(-a)).</br>")
    }
  }
  if(input$SRfunction=="hockeyStick"){
    if(input$covariates=="yes"){ 
      thetext=c(thetext, "R=S exp(a) exp(-f exp(d)) m^-c if S exp(a) < exp(b) </br>")
      thetext=c(thetext, "R=exp(b) exp(-f exp(d)) m^(-c)  otherwise </br>")
      thetext=c(thetext, "where f is stream flow and m is the marine condition index. </br>")
    }else{
      thetext=c(thetext, "R=S exp(a) if S exp(a) < exp(b) </br>")
      thetext=c(thetext, "R=exp(b) otherwise </br>")
    }
  }
  thetext=c(thetext,"R is the number of recruits and S is number of spawners.  This SR function is fit to the spawner-recruit data in the A and P file using vague priors on the parameters in order to compute the uncertainty in the estimates of the SR parameters.  </br></br>The A and P tab shows the input data from the A and P file.  The Posteriors tab summarizes the SR parameter uncertainty.")
  return(thetext)
}