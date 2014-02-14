\name{grpSummaryTable}
\docType{methods}
\alias{grpSummaryTable}
\title{
  Create a table of estimated group means and variability
}
\description{
  Create a table of estimated group means based on a fit by the \pkg{cg}
  package.
}
\usage{
grpSummaryTable(fit, mcadjust = FALSE, alpha = 0.05, display = "print",  \dots)
}
\arguments{
  \item{fit }{
    An fit object created with a 
    \code{\link{fit}} method from the \pkg{cg} package.
    The only class of object currently
    available is \code{\link{cgOneFactorFit}}, which is prepared by the
    \code{\link{fit.cgOneFactorData}} method.  
  }
  \item{mcadjust }{
    Do a multiple comparisons adjustment, based on the simultaneous
    inference capabilities of the \pkg{multcomp} package. See Details
    below. The default value is \code{FALSE}.
    If \code{mcadjust=TRUE} is specified, there will be a delay,
    usually just for a few seconds, due to computing time of the
    critical point in order to conduct the adjusted comparisons.
  }
  \item{alpha }{Significance level, by default set to \code{0.05}.
  }
  \item{display}{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr\code{cgOneFactorGrpSummaryTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{grpSummaryTable} object components.
      }
    }
  }
  \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. Currently, there is only one such specific method; see
    \code{\link{grpSummaryTable.cgOneFactorFit}} for any additional
    arguments that can be specified.
  }
}
\details{
  When \code{mcadjust=TRUE}, a status message of
  \code{"Some time may be needed as the critical point"}\cr
  \code{"from the multcomp::summary.glht function call is calculated"}
  is displayed at the console. This computed critical point
  is used for all subsequent p-value and confidence interval
  calculations.
}
\value{
  A method-specific \code{grpSummaryTable} object is returned.
  See the specific methods for discussion of return values.
}
\references{
  Hothorn, T., Bretz, F., Westfall, P., Heiberger, R.M., and
  Schuetzenmeister, A. (2010). The \code{multcomp} package.

  Hothorn, T., Bretz, F., and Westfall, P. (2008).
  "Simultaneous Inference in General Parametric Models",
  \emph{Biometrical Journal}, 50, 3, 346-363.  
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{grpSummaryTable.cgOneFactorFit}}
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.grpsumm <- grpSummaryTable(canine.fit)


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

gmcsfcens.grpsumm <- grpSummaryTable(gmcsfcens.fit)

}
\concept{group summary}
