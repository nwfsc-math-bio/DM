

library(shiny)

# Define UI for random distribution application 
fluidPage(
  
  # Application title
  titlePanel("Prior Parameters"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  fluidRow(
    column(3,
      h4("Production Prior"),
      sliderInput("pMode", 
                  "mode of log normal:", 
                  value = 20,
                  min = .001, 
                  max = 100),
      sliderInput("pSig", 
                  "sigma of lognormal:", 
                  value = 7,
                  min = .001, 
                  max = 10),
      sliderInput("pMax", 
                  "upper bound:", 
                  value = 40,
                  min = 1, 
                  max = 100)
    ),
    column(3,
      h4("Log Capacity Prior"),
      sliderInput("cMu", 
                  "mean:", 
                  value = 9,
                  min = .1, 
                  max = 20),
      sliderInput("cSig", 
                  "sigma:", 
                  value = 50,
                  min = 1, 
                  max = 100),
      sliderInput("cRange", 
                  "lower and upper bounds:", 
                  value = c(3,12),
                  min = .1, 
                  max = 20)
    ),
    column(3,
           h4("tau Prior"),
           sliderInput("tauShape", 
                       "log 10 shape:", 
                       value = -4,
                       min = -10, 
                       max = -1, step=.1),
           sliderInput("tauRate", 
                       "log 10 rate:", 
                       value = -4,
                       min = -10, 
                       max = -1, step=.1)
    ),
    column(3,
           h4("ms coef Prior"),
           sliderInput("msRange", 
                       "lower and upper bounds:", 
                       value = c(0,100),
                       min = 0, 
                       max = 100),br(),
           h4("flow coef Prior"),
           sliderInput("flowRange", 
                       "lower and upper bounds:", 
                       value = c(0,100),
                       min = 0, 
                       max = 100)
    )
    
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    fluidRow(
      plotOutput("plot")
    )
  )
