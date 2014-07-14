\name{fit}
\docType{methods}
\alias{fit}
\title{
  Fit models to data
}
\description{
  Fit data objects prepared by the \pkg{cg}
  package.
}
\usage{
fit(data, type, \dots)
}
\arguments{
  \item{data}{
    A data object prepared with a \code{\link{prepare}} call. This will involve a data
    frame and additional settings. 
  }
  \item{type }{Type of model to fit, represented by a character string.}
  \item{\dots }{
   Additional arguments, depending on the specific method written for
   the object. 
  }
}
\value{
  A method-specific \code{fit} object is returned.
  See the specific methods for discussion of return values.
} 
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{fit.cgOneFactorData}}
}

\examples{
## Unpaired Samples, One Factor
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data, type="rr")


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

## Paired Difference

data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
anorexiaFT.fit <- fit(data=anorexiaFT.data, type="rr")	

		  
}
\keyword{models}
