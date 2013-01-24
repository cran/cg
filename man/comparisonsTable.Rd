\name{comparisonsTable}
\docType{methods}
\alias{comparisonsTable}
\title{
  Create a Table of Comparisons amongst Groups
}
\description{
  Create a table of comparisons based on a fit by the \pkg{cg}
  package.
}
\usage{
comparisonsTable(fit, mcadjust = FALSE, type = "pairwisereflect",
 contrastmatrix = NULL, refgrp = NULL, alpha = 0.05, addpct = FALSE,
 display = "print", \dots)
}
\arguments{
  \item{fit }{
    A fit object created with a \code{\link{fit}} method from the \pkg{cg}
    package. \cr There is one class of objects that is currently available:
    \cr\code{\link{cgOneFactorFit}}, which is prepared by the 
    \cr\code{\link{fit.cgOneFactorData}} method. 
  }
  \item{mcadjust }{
    Do a multiple comparisons adjustment, based on the simultaneous
    inference capabilities of the \pkg{multcomp} package. See Details
    below. The default value is \code{FALSE}.
    If \code{mcadjust=TRUE} is specified, there will be a delay,
    usually just for a few seconds, due to computing time of the
    critical point in order to conduct the adjusted comparisons.
  }
  \item{type }{Can be one of four values:

    \describe{
      \item{\code{"pairwisereflect"}}{
	The default value, it calculates and lists all possible
	pairwise comparison permutations, as each pair order is
	included. In other words, Groups A vs. B and B vs. A will be
	included.
      }
      \item{\code{"pairwise"}}{Calculates and lists all possible
	pairwise comparison combinations. Once a pair such as Groups
	A vs. B is specified, then the reflective B vs. A is not
	included. So the number of comparisons is half that produced
	by \code{"pairwisereflect"}. The ordering of group levels in the
	\code{fit} object is used to determine which
	ordering is included and which is not. If all orderings are
	of interest, such as for \code{settings$endptscale=="log"} in the \code{fit}
	object, use the \code{"pairwisereflect"} value above.
      }
      \item{\code{"allgroupstocontrol"}}{Takes the value of
	\code{settings$refgrp} in the \pkg{cg} \code{fit}
	object, deems it the "control" group, and constructs
	pairwise comparisons of all other groups to it.
      }
      \item{\code{"custom"}}{Indicates that a custom matrix of
	comparisons will be constructed, and that matrix needs to be
	specified in the \code{contrastmatrix} argument.
      }
    }
  }
  \item{contrastmatrix }{Only relevant if \code{type="custom"} is
    specified. In that case, a numeric matrix with the number of
    rows equal to the number of comparisons of interest is needed. The number
    of columns must be equal to the number of estimated means. Each row in
    the matrix is assumed to represent a contrast of coefficients
    amongst the group means that defines the comparison of interest.
  }
  \item{refgrp }{If left at the default value of \code{NULL}, it will
    be set to the \code{settings$refgrp} value in the \pkg{cg} \code{fit}
    object. When set, it is deemed the "reference", or "control" group, so that
    pairwise comparisons of all other groups to it will be
    constructed when \code{type="allgroupstocontrol"}.
  }
  \item{alpha }{Significance level, by default set to \code{0.05}.
  }
  \item{addpct }{Only relevant if \code{settings$endptscale=="original"} in the 
    fit object. An column of percent differences is added for the
    comparisons, as a descriptive supplement to the original scale
    differences that are formally estimated.
  }
  \item{display}{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value, it calls a \code{print} method for the
	created \code{ComparisonsTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{ComparisonsTable} object components.
      }
    }
  }
  \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. See the method-specific documentation for additional
    details.
  }
}
\details{
  When \code{mcadjust=TRUE}, a status message of \code{"Some time may be
  needed as the critical point from the multcomp::summary.glht
  function call is calculated"} is displayed at the console. This computed critical point
  is used for all subsequent p-value and confidence interval
  calculations.

  The \pkg{multcomp} package provides a unified way to calculate
  critical points based on the comparisons of interest in a
  "family". Thus a user does not need to worry about choosing amongst
  the myriad names of multiple comparison procedures.
}
\value{
  A method-specific \code{comparisonsTable} object is returned.
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
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{comparisonsTable.cgOneFactorFit}}
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

canine.comps0 <- comparisonsTable(canine.fit)

canine.comps1 <- comparisonsTable(canine.fit,  mcadjust=TRUE,
                                   type="allgroupstocontrol", refgrp="CC")


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

gmcsfcens.comps <- comparisonsTable(gmcsfcens.fit)



}

\concept{comparisons}
\concept{multiplicity}
