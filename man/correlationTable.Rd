\name{correlationTable}
\docType{methods}
\alias{correlationTable}
\title{
  Compute Correlations
}
\description{
  Create a table of correlations of the data in a 
  \pkg{cg} data object.
}
\usage{
correlationTable(data, display = "print", \dots)
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
	created \code{correlationTable} object, which is
	a formatted text output of the table.
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{correlationTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Currently only one is valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether the correlation of
	the log transformed data should be calculated.
	If \code{logscale} is not
	specified (default), its value is taken from the
	\code{data} object's component in the \code{settings} slot.
      }
    }
  }
}
\value{
  A method-specific \code{correlationTable} object is returned.
  See the specific methods for discussion of return values. 
}
\seealso{
  \code{\link{correlationTable.cgPairedDifferenceData}}
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
correlationTable(anorexiaFT.data)

}
\concept{exploratory}
\concept{correlation}


