#' @title Calculate harvested recruits and escapement
#' @description Calculate harvested recruits and escapement
#' @param r1 a single run list output by the runModel function
#' @return A list with harvested recruits and escapement numbers.
calculateHarvested <- function(r1){
  bdat <- r1$bdat
  x <- r1$jagsOut$BUGSoutput$sims.list
  HR <- calcHarvest(bdat)
  dd <- dim(x$escapement)
  unharvestedRecruits <- array(NA,dim=dd[1:2]) 
  harvestedRecruits <- array(NA,dim=dd[1:2]) 
  
  for(i in 1:dd[1]){
    esc <- x$escapement[i,,]
    unharvestedRecruits[i,] <- apply(esc,1,sum)
    harvestedRecruits[i,] <- apply(esc*(HR/(1-HR)),1,sum)
  }
  
  unharvestedEsc <- array(NA,dim=c(dd[1],dd[2]+3))
  harvestedEsc <- array(NA,dim=c(dd[1],dd[2]+3))
  BY <- 1:bdat$lastYear
  calYrs <- 6:(bdat$lastYear+3)
  for(i in 1:dd[1]){
    esc <- x$escapement[i,,]
    hEsc <- esc*(HR/(1-HR))
    unharvestedEsc[i,calYrs] <- esc[calYrs-5,4] + esc[calYrs-4,3] + esc[calYrs-3,2]
    harvestedEsc[i,calYrs] <- hEsc[calYrs-5,4] + hEsc[calYrs-4,3] + hEsc[calYrs-3,2]
  }
  list(unharvestedRecruits=unharvestedRecruits, harvestedRecruits=harvestedRecruits,
       unharvestedEsc=unharvestedEsc,harvestedEsc=harvestedEsc)
}

