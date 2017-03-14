#' @title Get the simulations from a jags output list.
#' @param dmObj a saved DM object (list) from runModel() with the objects "input","dat","result", "bdat" and "tDat"
#' @return a list of vectors of the posterior draws for each parameter.
getPostDraws <- function(dmObj) dmObj$jagsOut$BUGSoutput$sims.list
