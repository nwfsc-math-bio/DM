\name{DMMain}
\alias{DMMain}
\title{ Estimate SR parameters from A and P file using DM and save .rav file and .RData file.}
\description{
This function takes the input from an A & P files, inputs that into DM and creates a RAV file that can be used for running the VRAP simulations. The .rav file can then input into VRAP to generate the RER.
Current version has no uncertainty in management error and base exploitation rate must be included
as a parameter. This could be calculated with the SR params, maturation rates, natMort, and harvest.
}
\usage{
DMVRAP(
  a.and.p.file=NULL, 
  RData.file=NULL, 
  output.file=NULL, 
  srFunc=NULL, 
  covariates=NULL,
  rav.options=list(
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
  \item{a.and.p.file }{comma-delimited data exported from the A&P Excel files}
  \item{RData.file}{ debugging option.  allows us to run code by importing directly from saved data }
  \item{output.file}{ filename of file where DM output (.csv and .Rdata) will be saved.  }
  \item{rav.file}{ filename of the RAV file to be saved.  }
  \item{srFunc}{ Hockey-stick, Beverton-Holt, or Ricker }
  \item{covariates}{ whether to include the streamflow covariate }
  \item{simInd}{ which MCMC run to use. }
  \item{numRuns}{ number of runs used in RER simulations. }
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
  Eli Holmes, NOAA, Seattle, USA.  

  eli(dot)holmes(at)noaa(dot)gov
}
