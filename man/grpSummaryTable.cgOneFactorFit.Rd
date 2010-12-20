\name{grpSummaryTable.cgOneFactorFit}
\docType{methods}
\alias{grpSummaryTable.cgOneFactorFit}
\alias{cgOneFactorGrpSummaryTable}
\alias{cgOneFactorGrpSummaryTable-class}
\alias{grpSummaryTable,cgOneFactorFit-method}
\title{
  Create a table of estimated group means and variability with a cgOneFactorFit object.
}
\description{
 Create a table of estimated group means based on the cgOneFactorFit
 object. Standard errors and confidence
 intervals are added. A cgOneFactorGrpSummaryTable class object is created.    
}
\usage{
\S4method{grpSummaryTable}{cgOneFactorFit}(fit, mcadjust=FALSE, alpha=0.05, display="print",  ...)
}
\arguments{
  \item{fit }{
     A fit object of class \code{\link{cgOneFactorFit}}.  
  }
  \item{mcadjust }{
    Do a multiple comparisons adjustment, based on the simultaneous
    inference capabilities of the \pkg{multcomp} package. See Details
    below. The default value is \code{FALSE}.
    If \code{mcadjust=TRUE} is specified, there will be a delay,
    usually just for a few seconds, due to computing time of the
    critical point in order to calculate the confidence intervals.
  }
  \item{alpha }{Significance level, by default set to \code{0.05}.
  }
  \item{display}{One of three valid values:
	
    \describe{
      \item{\code{"print"}}{
	The default value, it calls a \code{print} method for the
	created \cr\code{cgOneFactorGrpSummaryTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgOneFactorGrpSummaryTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorFit} \code{fit} objects that have 
	classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Group summary tables based on both the ordinary 
	    classical least squares and resistant & robust fits are
	    performed. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a group summary table based on the ordinary 
	    classical least squares \code{olsfit} fit is performed.
	  }
	  \item{\code{"rronly"}}{Only a group summary table based on the resistant and
	    robust \code{rrfit} fit is performed.
	  }
	}
      }
    }
   For other possible \code{cgOneFactorFit} fit components such as
   accelerated failure time or unequal variance models, the \code{model}
   argument is not relevant, and the appropriate group summary table will
   be calculated for these model types.
 }
}
\value{
  Creates an object of class  \code{cgOneFactorGrpSummaryTable}, with the
  following slots:

  \describe{
    \item{\code{ols.grps}}{The table of group estimates based on the
      \code{olsfit} component of the \code{\link{cgOneFactorFit}},
      unless \code{model="rronly"} is specified. In that case the slot
      value is \code{NULL}. Will not be appropriate in
      the case where a valid \code{aftfit} component is present in the
      \code{\link{cgOneFactorFit}} object. See below for the data frame structure
      of the table.
    }
    \item{\code{rr.grps}}{The table of group estimates based on the
      \code{rrfit} component of the \code{\link{cgOneFactorFit}}
      object, if a valid resistant & robust fit object is present.
      If \code{rrfit} is a simple character value of
      \code{"No fit was selected."}, or \code{model="olsonly"} was
      specified, then the value is \code{NULL}. See below for the data frame structure
      of the table.
    }
    \item{\code{aft.grps}}{The table of group estimates based on the
      \code{aftfit} component of the \code{\link{cgOneFactorFit}}
      object if a valid accelerated failure time fit object is present.
      If \code{aftfit} is a simple character value of
      \code{"No fit was selected."}, then the value is \code{NULL}.
      See below for the data frame structure
      of the table.
    }
    \item{\code{uv.grps}}{The table of group estimates based on the
      \code{uvfit} component of the \code{\link{cgOneFactorFit}}
      object if a valid unequal variances fit object is present.
      See below for the data frame structure
      of the table.
    }
    \item{\code{settings}}{A list of settings carried from the
      \code{\link{cgOneFactorFit}} \code{fit} object, and the addition
      of some specified arguments in the method call above: \code{alpha}
      and \code{mcadjust}. These are used
      for the \code{\link{print.cgOneFactorGrpSummaryTable}} method,
      invoked for example when
      \code{display="print"}.
    }
  }

  The data frame structure of the comparisons table in a \code{*.comprs}
  slot consists of \code{row.names} that specify group name (factor
  level), and these columns:

  \describe{
    \item{\code{estimate}}{The estimated group mean. If \code{settings$endptscale=="log"} in the
      \code{fit} object, this will back-transformed to a geometric mean.
    }
    \item{\code{se}}{The estimated standard error of the group mean
      \code{estimate}. If \code{settings$endptscale=="log"} in the
      \code{fit} object, this estimate will be based on the Delta
      method, and will begin to be a poor approximation when the
      standard error in the logscale exceeds 0.50. 
    }
    \item{\code{lowerci}}{The lower 100 * (1-\code{alpha}) \% confidence limit of the
      group mean \code{estimate}. With the default \code{alpha=0.05},
      this is 95\%. If \code{settings$endptscale=="log"} in the
      \code{fit} object, the confidence limit is first computed in the
      logarithmic scale of analysis, and then back-transformed to the
      original scale.
    }
    \item{\code{upperci}}{The upper 100 * (1-\code{alpha}) \% confidence limit of the
      difference \code{estimate}. With the default \code{alpha=0.05},
      this is 95\%. If \code{settings$endptscale=="log"} in the
      \code{fit} object, the confidence limit is first computed in the
      logarithmic scale of analysis, and then back-transformed to the
      original scale.
    }
  }
  
}
\details{
  When \code{mcadjust=TRUE}, a status message of \code{"Some time may be
  needed as the critical point from the multcomp::summary.glht function
  call is calculated"} is displayed at the console. This computed critical point
  is used for all subsequent confidence interval  calculations.

  The \pkg{multcomp} package provides a unified way to calculate
  critical points based on the comparisons of interest in a
  "family". Thus a user does not need to worry about choosing amongst
  the myriad names of multiple comparison procedures.
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

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

grpSummaryTable(canine.fit)

grpSummaryTable(canine.fit, mcadjust=TRUE, model="olsonly")


data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

grpSummaryTable(gmcsfcens.fit)

}

\concept{group summary}
\concept{multiplicity}
