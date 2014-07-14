\name{globalTest}
\docType{methods}
\alias{globalTest}
\title{
  Perform a global test of significance
}
\description{
  Generic function to perform a global test of significance on a fit by the \pkg{cg}
  package.
}
\usage{
globalTest(fit, display="print", \dots)
}
\arguments{
  \item{fit }{
    A fit object created by a \code{\link{fit}} method from the \pkg{cg}
    package. The only class of object currently
    available is \code{\link{cgOneFactorFit}}, which is prepared by the
    \code{\link{fit.cgOneFactorData}} method.  
  }
  \item{display}{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{globalTest} object, which is
	formatted text output of the test p-values.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{globalTest} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. See the method-specific documentation for additional
    details.
  }
}
\value{
  A method-specific \code{globalTest} object is returned.
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
  \code{\link{globalTest.cgOneFactorFit}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.globalTest <- globalTest(canine.fit)

data(gmcsfcens) 
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

gmcsfcens.globalTest <- globalTest(gmcsfcens.fit)

}
\concept{global test}
