########################################################################################
# create a mod1.txt which contains the model for BUGS
########################################################################################
writeBUGScode <- function(input){    
  # Define SR function
  tmpStr <- switch(input$SRfunction,
                   hockeyStick = "min(prod*totalSpawnersAge3to5[year],exp(logCap))",
                   ricker = "prod*totalSpawnersAge3to5[year]*exp(-totalSpawnersAge3to5[year]/exp(logCap))",
                   bevertonHolt = "totalSpawnersAge3to5[year]/(totalSpawnersAge3to5[year]/exp(logCap) + 1/prod)"
  ) 
  SRstr <- paste(tmpStr,ifelse(input$includeMarineSurvival=="yes","*pow(marineSurvivalIndex[year],msCoef)",""),ifelse(input$includeFlow=="yes","*exp(-flowCoef*flow[year])",""),sep="")
  
  # write model to file
  mod1 <- paste("
	model
	{
		# likelihood
		for(year in (firstYear+5):(lastYear+3)){
			logTotalWildEscapementAge3to5[year] ~ dnorm(logPredictedWildEscapementAge3to5[year],tau)
			logPredictedWildEscapementAge3to5[year] <- log(escapement[year-3,2] + escapement[year-4,3] + escapement[year-5,4])
		}
		
		for(year in firstYear:lastYear){
			# generate age 2 values
			AEQrecruitsFromSRrelationship[year] <- ",SRstr,"
			AEQrecruitsFromExpansionFactor[year] <- AEQrecruitsFromSRrelationship[year] / (AEQ[year] * (1 - naturalMort[2]))
			postMortCohort[year,1] <- AEQrecruitsFromExpansionFactor[year] * (1 - naturalMort[2]) * (1 - mixedMaturityFishingRate[year,1])
			matureRun[year,1] <- postMortCohort[year,1]*maturationRate[year,1]
			postMaturationCohort[year,1] <- postMortCohort[year,1] - matureRun[year,1]
			
			# age 3-5 built on age 2 fish 
			for(i in 2:4){
				postMortCohort[year,i] <- postMaturationCohort[year,i-1] * (1 - naturalMort[i+1]) * (1 - mixedMaturityFishingRate[year,i])
				matureRun[year,i] <- postMortCohort[year,i]*maturationRate[year,i]
				postMaturationCohort[year,i] <- postMortCohort[year,i] - matureRun[year,i]
				escapement[year,i] <- matureRun[year,i] * (1 - matureFishingRate[year,i]) * (1 - preSpawnMortRate[year,i])
			}
		}	
	
	# priors
		#prod ~ dunif(prodPrior[1],prodPrior[2])
		#logCap ~ dunif(logCapPrior[1],logCapPrior[2])
		prod ~ dlnorm(prodPrior[1],prodPrior[2])I(prodPrior[3],prodPrior[4])
		logCap ~ dnorm(logCapPrior[1],logCapPrior[2])I(logCapPrior[3],logCapPrior[4])
		msCoef ~ dnorm(msCoefPrior[1],msCoefPrior[2])I(msCoefPrior[3],msCoefPrior[4])
		flowCoef ~ dnorm(flowCoefPrior[1],flowCoefPrior[2])I(flowCoefPrior[3],flowCoefPrior[4])
		tau ~ dgamma(tauPrior[1],tauPrior[2])
	
	}
	",sep="")
  
  write(mod1, file="mod1.txt")
}