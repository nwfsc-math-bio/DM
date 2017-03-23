#' @title Shiny app to run DM and VRAP
#' @aliases DMVRAP
#' @description This function takes the input from an A & P files, runs DM to estimate SR parameters and saves a .rav file and .RData file. The .rav file can be downloaded or directly used for running the VRAP simulations by clicking the VRAP tab.
DM=function(){
  shiny:::runApp(system.file('appdir/DMVRAP', package='DM'))
}
