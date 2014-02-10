\name{varianceTable}
\docType{methods}
\alias{varianceTable}
\title{
  Compute Variance Components
}
\description{
  Create a table of variances from a  
  \pkg{cg} fit object.
}
\usage{
varianceTable(fit, display = "print", \dots)
}
\arguments{
  \item{fit }{
    A fit object created and prepared (see \code{\link{prepare}})
    using the \pkg{cg} package.   
  }
  \item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{varianceTable} object, which is
	a formatted text output of the table.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{varianceTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently none are valid. 
  }
}
\value{
  A method-specific \code{varianceTable} object is returned.
  See the specific methods for discussion of return values. 
}
\seealso{
  \code{\link{varianceTable.cgPairedDifferenceFit}}
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)
anorexiaFT.fit <- fit(data=anorexiaFT.data)	
varianceTable(anorexiaFT.fit)

}
\concept{variance components}


