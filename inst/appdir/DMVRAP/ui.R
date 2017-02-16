#.libPaths(c("/usr/lib64/R/shiny_library",.libPaths()))
library("appFrame")

source("common.R")  ## import EXAMPLES, shared with server.R

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      tags$script(includeHTML("www/timer2.js")),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setwaitmsg',
                                     function(msg) {
                                     $('#waitmsg').html(msg);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setmsgclass',
                                     function(msg) {
                                     $('#spinnerhost').attr('class',msg);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setprogressdisplay',
                                     function(disp) {
                                     $('#progresscounter').css('display',disp);
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('setusermsg',
                                     function(msg) {
                                     $('#msghostmsg').html(msg);
                                     $('#msghost').attr('class', 'msgdisplay');
                                    })"
             )
      ),
      tags$script(
        HTML("Shiny.addCustomMessageHandler('clearusermsg',
                                     function(msg) {
                                     $('#msghostmsg').html('');
                                     $('#msghost').attr('class', 'nomsgdisplay');
                                    })"
             )
      )
    ),
    appFrameHeaderFixed(),
    headerPanel("Dynamic Model + VRAP 1.1"),
    conditionalPanel(
      condition=paste(
        "(updateBusy()) || ",
        "$('html').attr('class')=='shiny-busy' ||",
        "$('#spinnerhost').hasClass('busy')",
        sep=""),
      div(" "),
      div(id="spinnerhost",
          class="notbusy",
          p(id="waitmsg", " "),
          img(src="spinner.gif",
              alt=paste("Busy, please wait",
                        "(Close browser window to stop job)"
                        )
              ),
          p(' '),
          div(id="progress")
          )
    ),
    conditionalPanel(
      condition="$('#msghost').hasClass('msgdisplay')",
      div(id="msghost",
          class="nomsgdisplay",
          p(id="msghostmsg"," "),
          actionButton("msgclosebtn","Dismiss message")
          )
    ),
    tabsetPanel(
      id="DM_VRAP_tabs",
      tabPanel(
        title = "DM",
        value = "dmtab",
        sidebarLayout(
          sidebarPanel(
            div(id="spacer"," ",style="min-height:35px;float:left"),
            div(uiOutput("runbutton")),
            tags$br(),
            selectInput("type",
                        paste("Select an input type",
                              "(A&P XLSX or a demo file):"),
                        list("A&P XLSX file (.xlsx)" = "XLSX",
                             "DM model file (.RData)" = "RData",
                             "Demo A & P file" = "Demo"),
                        selected="XLSX"
                        ),
            conditionalPanel(
              "input.type == 'XLSX'",
              uiOutput("xlsxFileInput")
            ),
            conditionalPanel(
              "input.type == 'RData'",
              uiOutput("rdFileInput"),
              tags$h5(includeHTML("html/rdataclickrun.html"))
            ),
            conditionalPanel(
              paste("input.type == 'AP' || input.type == 'XLSX' ",
                    "|| input.type == 'Demo'",sep=''), 
              selectInput("srFunc", "SR Function:",
                          list("Ricker" = "ricker", 
                               "Beverton-Holt" = "bevertonHolt", 
                               "Hockey Stick" = "hockeyStick")),
              selectInput("covariates",
                          "Include Marine Survival and Stream Flow covariates?",
                          list("No" = "no", 
                               "Yes" = "yes"))),
            conditionalPanel(
              "input.type == 'XLSX'"
              ## tags$h5(includeHTML("html/xlsxclickrun.html"))
            ),
            conditionalPanel(
              "input.type == 'Demo'"
              ## tags$h5(includeHTML("html/democlickrun.html"))
            )
          ),
          mainPanel(
            tabsetPanel(
              id="output",
              tabPanel("Info", uiOutput("info")),
              tabPanel(HTML("<center>DM<br/>A&P Data</center>"),
                       div(plotOutput("plot1")),
                       br(), uiOutput("caption1")),
              tabPanel(HTML("<center>DM<br/>Posteriors</center>"), 
                       div(plotOutput("plot2"), width="90%"), br(),
                       uiOutput("caption2")),
              tabPanel("Downloads", uiOutput("downloadtab")),
              tabPanel("Priors",
                       source("inputpriors.R")),
              tabPanel("Help", includeHTML("html/dmmain_help.html"),
                       value="dmmainhelp"),
              selected="dmmainhelp"
            )
          )
        )
      ),
      tabPanel(
        title="VRAP",
        value="vraptab",
        sidebarLayout(
          sidebarPanel(
            div(uiOutput("vraprunbutton")),
            tags$br(),
            div(uiOutput("vraptypeselect")),
            conditionalPanel(
              "input.vraptype == 'RAV'",
              uiOutput("ravFileInput")
            ),
            conditionalPanel(
              "input.vraptype == 'demo'",
              selectInput("vrapdemofile", "Select demo file:", EXAMPLES)
            ),
            conditionalPanel(
              paste("input.vraptype=='RAV' |",
                    "input.vraptype=='demo' |",
                    "input.vraptype=='currentRData'"),
              radioButtons(
                'vrapnumruns',
                'Number of runs per simulation',
                c('1'=1, '10'=10, '100'=100, '1000'=1000,
                  'Use NRuns from RAV file' = -1),
                selected=-1)
            ),
            conditionalPanel(
              "input.vraptype == 'demo'",
              selectInput( "vrapdemodownload", "Download demo RAV files:",
                          EXAMPLES),
              downloadButton('downloadDemo', 'Download Demo File')
              )
          ),
          mainPanel(
            tabsetPanel(
              id="vrapoutput",
              tabPanel(HTML("<center>VRAP<br/>Summary</center>"),
                       uiOutput("VRAPsum")),
              tabPanel(HTML("<center>VRAP<br/>Brood Year</center>"),
                       uiOutput("VRAPbyr")),
              tabPanel(HTML("<center>VRAP<br/>Escapement</center>"),
                       uiOutput("VRAPesc")),
              tabPanel("Downloads", uiOutput("vrapdownloadtab")),
              tabPanel("Help",
                       includeHTML("html/vrapmain_help.html"))
            )
          )
        )
      )
    ),

    appFrameFooterFixed(displayAppsURL="../..")
  )
)

