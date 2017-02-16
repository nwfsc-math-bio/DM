\name{createRAVfile}
\alias{createRAVfile}
\title{ Create RAV file from output of DM }
\description{
This function takes the output from the DM model and creates a RAV file that can be used for 
   running the VRAP simulations. The parameter simInd indicates which MCMC sim to use.
This takes the information from the DM output .Rdata file which has information from the original
A and P file and the output of the MCMC sims used in DM. 
Current version has no uncertainty in management error and base exploitation rate must be included
as a parameter. This could be calculated with the SR params, maturation rates, natMort, and harvest.
}
\usage{
createRAVfile(
  dat,
  input,
  result,
  tDat,
  filename="temp_rav.rav",
  options=list(
     simInd=1,
     numRuns = 1000,
     numYears = 25,
     minAge = 2,
     maxAge = 5,
     modelDepensation = "NO",
     depensation = 300,
     QETcritical = 63,
     depensPar3 = 1 ,
     lowerEscThreshold = 200, 
     recruitsFromAdultSpawners = "YES",
     SRvariation = "YES",
     baseExploitationRate = 0.70)
)  
}
  
  \arguments{
  \item{dat, input, result, tDat}{ Objects output by DM. }
  \item{simInd}{ which MCMC sim to use. }
  \item{fileName}{ filename of the RAV file to be saved.  }
  \item{numRuns}{ number of runs used in the RER simulations. }
  \item{numYears}{ length of forecast used in the RER simulations. }
  \item{minAge}{   }
  \item{maxAge}{   }
  \item{modelDepensation}{  }
  \item{depensation}{  }
  \item{lowerEscThreshold}{ Lower escapement threshold; Norma Jean Sands: from past or proposed 4-d ruling.  }
  \item{recruitsFromAdultSpawners}{   }
  \item{SRvariation}{   }
  \item{baseExploitationRate}{ This can be calculated from data (see Norma's RAV file calculations). But for now we make it a parameter. }
  }

\value{
  A ascii file with the extension .rav to be used in VRAP for RER calculations.
}
\author{ 
  Martin Liermann, NOAA, Seattle, USA.  

  martin(dot)liermann(at)noaa(dot)gov
}
