\name{descriptiveTable}
\docType{methods}
\alias{descriptiveTable}
\title{
  Compute Descriptive Summary Statistics of Groups
}
\description{
  Create a table of quantiles and other summary statistics of the data in a 
  \pkg{cg} data object.
}
\usage{
descriptiveTable(data, display = "print", \dots)
}
\arguments{
  \item{data }{
    A data object created and prepared (see \code{\link{prepare}})
    using the \pkg{cg}
    package.   
  }
  \item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{descriptiveTable} object, which is
	a formatted text output of the table.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{descriptiveTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently only one is valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the geometric means and
	standard errors should be included in the summary.
	If \code{logscale} is not
	specified (default), its value is taken from the
	\code{\link{data}}
	object.
      }
    }
  }
}
\value{
  A method-specific \code{descriptiveTable} object is returned.
  See the specific methods for discussion of return values. 
}
\seealso{
  \code{\link{descriptiveTable.cgOneFactorData}}
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
descriptiveTable(canine.data)

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
descriptiveTable(gmcsfcens.data)

## Paired Difference Data

data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)

descriptiveTable(anorexiaFT.data)

}
\concept{exploratory}
\concept{descriptive}


