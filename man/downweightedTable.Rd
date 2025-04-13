\name{downweightedTable}
\docType{methods}
\alias{downweightedTable}
\title{
  Create a table of downweighted observations from a Resistant & Robust fit.
}
\description{
  Create a table of downweighted observations in a resistant & robust
  fit with the \pkg{cg} package.
}
\usage{
downweightedTable(fit, cutoffwt, display="print", \dots)
}
\arguments{
  \item{fit }{
    A fit object created with a \code{\link{fit}} method from the \pkg{cg}
    package.
  }
  \item{cutoffwt }{
    It has no default and must be specified as a numeric between 0 and 1
    exclusive. It is a threshold. All
    observations that fall \emph{beneath} the threshold will be
    identified. For example, a \code{cutoffwt=0.90} will yield
    those observations that were downweighted by at least 10\%.
  }
  \item{display}{One of three valid values:

    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \code{downweightedTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{downweightedTable} components.
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
  A method-specific \code{downweightedTable} object is returned.
  See the specific methods for discussion of return values.
}
\seealso{
  \code{\link{downweightedTable.cgOneFactorFit}}, \pkg{MASS}::\code{\link[MASS]{rlm}}
}
\references{
  Venables, W. N. and Ripley, B. D. (2002),
  \emph{Modern Applied Statistics with S.}  Fourth edition. Springer.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\examples{
#### One Factor data

data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.dwtable <- downweightedTable(canine.fit, cutoff=0.95)

downweightedTable(canine.fit, cutoff=0.75) ## No observation
                                           ## downweighted at least 25\%

## Paired Difference data

anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)

downweightedTable(anorexiaFT.fit, cutoffwt=0.25) ## No observation

downweightedTable(anorexiaFT.fit, cutoffwt=0.75) ## downweighted at least 25\%

}
\concept{weights}
\concept{resistance}
\concept{robust}




