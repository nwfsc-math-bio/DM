########################################################################################
##
########################################################################################
calcInitVals <- function(result, dat, input){
  estimate <- result$estimate
  function(){ 
    list(
      prod = min(c(exp(estimate[1]),49)),
      logCap = min(estimate[2],19),
      msCoef = min(exp(estimate[3]),99),
      flowCoef = min(exp(estimate[4]),99),
      tau = 1/(SSTfuncDM(result$estimate,selectSR(input$SRfunction),dat,input)/(input$lastYear-input$firstYear+1))
    )
  }
}
