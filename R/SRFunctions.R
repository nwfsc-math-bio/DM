######################################################
## SR Functions
## Takes S and returns R
## Plus functions for selecting the right SR functions
######################################################
ricker <- function(S,flow,marineInd,p){ # with flow
  S*exp(p[1])*exp(-S/exp(p[2]) - exp(p[4])*flow)*marineInd^exp(p[3])
}

bevertonHolt <- function(S,flow, marineInd,p){
  S/(S*exp(-p[2])+exp(-p[1]))*exp(-exp(p[4])*flow)*marineInd^exp(p[3])
}

hockeyStick <- function(S, flow, marineInd, p){
  ifelse(exp(p[1])*S<exp(p[2]),exp(p[1])*S,exp(p[2]))*exp(-exp(p[4])*flow)*marineInd^exp(p[3])
}

selectSR <- function(x){
  if(x=="ricker") ff <- ricker
  else if(x=="bevertonHolt") ff <- bevertonHolt
  else if(x=="hockeyStick") ff <- hockeyStick
  else{
    print("ERROR: not a recognized SR function in function selectSR")
    break
  }
  ff
}

# parameterized in terms of 
selectSmsy <- function(funcName){
  if(funcName=="ricker") ff <- function(prod,cap) log(prod)*cap*(0.5-0.07*log(prod)) # approximation from Hilborn
  else if(funcName=="bevertonHolt") ff <- function(prod,cap) b*sqrt(1/prod)-cap/prod
  else if(funcName=="hockeyStick") ff <- function(prod,cap) cap/prod
  else{
    print(c("ERROR: ",funcName, " not a recognized SR function in function selectSmsy"))
    break
  }
  ff
}