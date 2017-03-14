

library(shiny)

# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    input$pMode
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    par(mfrow=c(2,3))
    x <- seq(0,100,.1)
    ix <- rep(1, length(x)); ix[x>input$pMax]=0
    plot(x, dlnorm(x,log(input$pMode)+input$pSig^2,input$pSig)*ix, 
         main='Production Prior',xlab='Production',ylab='Density',type='l',bty='n')
    
    x <- seq(0,100,.1)
    ix <- rep(1, length(x)); ix[x>input$cRange[2]]=0; ix[x<input$cRange[1]]=0
    plot(x, dnorm(x,input$cMu,input$cSig)*ix, 
         main='Log Capacity Prior',xlab='Log Capacity',ylab='Density',type='l',bty='n')

    x <- seq(1e-5,1e-2,1e-5)
    plot(x, dgamma(x,10^input$tauShape,10^input$tauRate), 
         main='tau Prior',xlab='tau',ylab='Density',type='l',bty='n')
    
    x <- seq(0,100,.01)
    plot(x,dunif(x,min=input$msRange[1],max=input$msRange[2]),
         main='ms coef Prior',xlab='ms coef',ylab='Density',type='l',bty='n')

    x <- seq(0,100,.01)
    plot(x,dunif(x,min=input$flowRange[1],max=input$flowRange[2]),
         main='ms coef Prior',xlab='flow coef',ylab='Density',type='l',bty='n')
  })
  
}

