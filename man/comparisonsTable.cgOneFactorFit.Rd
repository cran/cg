\name{comparisonsTable.cgOneFactorFit}
\docType{methods}
\alias{comparisonsTable.cgOneFactorFit}
\alias{comparisonsTable,cgOneFactorFit-method}
\alias{cgOneFactorComparisonsTable-class}
\alias{cgOneFactorComparisonsTable}
\title{
  Create a table of comparisons amongst groups with the cgOneFactorFit object
}
\description{
  Create a table of comparisons based on the cgOneFactorFit
  object. Pairwise or custom specified contrasts are estimated and
  tested.  A cgOneFactorComparisonsTable class object is created.    
}
\usage{
\S4method{comparisonsTable}{cgOneFactorFit}(fit, type="pairwisereflect",
 alpha=0.05, addpct=FALSE, display="print", \dots)
}
\arguments{
  \item{fit }{
    An object of class \code{\link{cgOneFactorFit}}.  
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
	of interest, such as for \code{settings$endptscale=="log"} in the fit
	objects, use the \code{"pairwisereflect"} value above.
      }
      \item{\code{"allgroupstocontrol"}}{Takes the value of
	\code{settings$refgrp} in the \pkg{cg} \code{fit}
	object, deems it the "control" group, and constructs
	pairwise comparisons of all other groups to it.
      }
      \item{\code{"custom"}}{Indicates the a custom matrix of
	comparisons will be constructed, and that matrix needs to be
	specified in the \code{contrastmatrix} argument.
      }
    }
  }
  \item{alpha }{Significance level, by default set to \code{0.05}.
  }
  \item{addpct }{Only relevant if \code{settings$endptscale=="original"} in the 
    fit object. An column of percent differences is added for the
    comparisons, as a descriptive supplement to the original scale
    differences that are formally estimated.
  }
  \item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value; It calls a \code{print} method for the
	created \cr\code{cgOneFactorComparisonsTable} object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{cgOneFactorComparisonsTable} components.
      }
    }
  }
  \item{\dots }{
    Additional arguments.  
    \describe{
      \item{\code{mcadjust} }{
	Do a multiple comparisons adjustment, based on the simultaneous
	inference capabilities of the \pkg{multcomp} package. See Details
	below. The default value is \code{FALSE}.
	If \code{mcadjust=TRUE} is specified, there will be a delay,
	usually just for a few seconds, due to computing time of the
	critical point in order to conduct the adjusted comparisons.
      }
      \item{\code{contrastmatrix} }{Only relevant if \code{type="custom"} is
	specified. In that case, a numeric matrix with the number of
	rows equal to the number of comparisons of interest. The number
	of columns must be equal to the number of group means. Each row in
	the matrix is assumed to represent a contrast of coefficients
	amongst the groups that defines the comparison of interest.
      }
      \item{\code{refgrp} }{If left at the default value of \code{NULL}, it will
	be set to the \code{settings$refgrp} value in the \pkg{cg} \code{fit}
	object. When set, it is deemed the "reference", or "control" group, so that
	pairwise comparisons of all other groups to it will be
	constructed when \code{type="allgroupstocontrol"}.
      }
      \item{\code{model} }{For \code{cgOneFactorFit} \code{fit} objects that have 
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Comparison tables based on both the ordinary 
	    classical least squares and resistant & robust fits are
	    created. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a comparison table based on the ordinary 
	    classical least squares \code{olsfit} fit slot is performed.
	  }
	  \item{\code{"rronly"}}{Only a comparison table based on the resistant and
	    robust \code{rrfit} fit slot is performed.
	  }
	}
      }
    }
    For other possible \code{cgOneFactorFit} fit components such as
    accelerated failure time or unequal variance models, the \code{model}
    argument is not relevant, and the appropriate comparisons table will
    be calculated for these model types.
  }
}
\details{
  When \code{mcadjust=TRUE}, a status message of \code{"Some time may be
    needed as the critical point"}\cr
    \code{"from the multcomp::summary.glht function
    call is calculated"} is displayed at the console. This computed critical point
    is used for all subsequent p-value and confidence interval
    calculations.
    
    The \pkg{multcomp} package provides a unified way to calculate
    critical points based on the comparisons of interest in a
    "family". Thus a user does not need to worry about choosing amongst
    the myriad names of multiple comparison procedures.
  }
  \value{
    Creates an object of class  \code{cgOneFactorComparisonsTable}, with the
    following slots:
    
    \describe{
      \item{\code{ols.comprs}}{The table of comparisons based on the
	\code{olsfit} component of the \code{\link{cgOneFactorFit}},
	unless \code{model="rronly"} is specified. In that case the slot
	value is \code{NULL}. Will not be appropriate in
	the case where a valid \code{aftfit} component is present in the
	\code{\link{cgOneFactorFit}} object. See below for the data frame structure
	of the table.
      }
      \item{\code{rr.comprs}}{The table of comparisons based on the
	\code{rrfit} component of the \code{\link{cgOneFactorFit}}
	object, if a valid resistant & robust fit object is present.
	If \code{rrfit} is a simple character value of
	\code{"No fit was selected."}, or \code{model="olsonly"} was
	specified, then the value is \code{NULL}. See below for the data frame structure
	of the table.
      }
      \item{\code{aft.comprs}}{The table of comparisons based on the
	\code{aftfit} component of the \code{\link{cgOneFactorFit}}
	object if a valid accelerated failure time fit object is present.
	If \code{aftfit} is a simple character value of
	\code{"No fit was selected."}, then the value is \code{NULL}.
	See below for the data frame structure
	of the table.
      }
      \item{\code{uv.comprs}}{The table of comparisons based on the
	\code{uvfit} component of the \code{\link{cgOneFactorFit}}
	object if a valid unequal variances fit object is present.
	The error degrees of freedom for each comparison estimate and
	test is individually estimated
	with a Satterthwaite approximation. See below for the data frame structure
	of the table.
      }
      \item{\code{settings}}{A list of settings carried from the
	\code{\link{cgOneFactorFit}} \code{fit} object, and the addition
	of some specified arguments in the method call above: \code{alpha},
	\code{mcadjust}, \code{type}, and \code{addpct}. These are used
	for the \code{\link{print.cgOneFactorComparisonsTable}} method,
	invoked for example when
	\code{display="print"}.
      }
    }

    The data frame structure of the comparisons table in a \code{*.comprs}
    slot consists of \code{row.names} that specify the comparison of the
    form A vs. B, and these columns:

    \describe{
      \item{\code{estimate}}{The difference in group means in the
	comparison: A vs. B. If \code{settings$endptscale=="log"} in the
	\code{fit} object, this will be back-transformed to a percent
	difference scale.
      }
      \item{\code{se}}{The estimated standard error of the difference
	\code{estimate}. If \code{settings$endptscale=="log"} in the
	\code{fit} object, this estimate will be based on the Delta
	method, and will particularly begin to be a poor approximation when the
	standard error in the logscale exceeds 0.50. 
      }
      \item{\code{lowerci}}{The lower 100 * (1-\code{alpha}) \% confidence limit of the
	difference \code{estimate}. With the default \code{alpha=0.05},
	this is 95\%. If \code{settings$endptscale=="log"} in the
	\code{fit} object, the confidence limit is first computed in the
	logarithmic scale of analysis, and then back-transformed to a percent
	difference scale.
      }
      \item{\code{upperci}}{The upper 100 * (1-\code{alpha}) \% confidence limit of the
	difference \code{estimate}. With the default \code{alpha=0.05},
	this is 95\%. If \code{settings$endptscale=="log"} in the
	\code{fit} object, the confidence limit is first computed in the
	logarithmic scale of analysis, and then back-transformed to a percent
	difference scale.
      }
      \item{\code{pval}}{The computed p-value from the test of the difference \code{estimate}.
      }
      \item{\code{meanA} or \code{geomeanA}}{The estimated mean for the
	left hand side "A" of the A vs. B  comparison.
	If \code{settings$endptscale=="log"} in the
	\code{fit} object, this is a back-transform to the original scale,
	and therefore is a geometric mean, and will be labelled
	\code{geomeanA}.
	Otherwise it is the arithmetic mean and labelled \code{meanA}.
      }
      \item{\code{seA}}{The estimated standard error of the \code{meanA}
	\code{estimate}. If \code{settings$endptscale=="log"} in the
	\code{fit} object, this estimate will be based on the Delta
	method, and will particularly begin to be a poor approximation when the
	standard error in the logscale exceeds 0.50. 
      }
      \item{\code{meanB} or \code{geomeanB}}{The estimated mean for the
	right hand side "B" of the A vs. B comparison.
	If \code{settings$endptscale=="log"} in the
	\code{fit} object, this is a back-transform to the original scale,
	and therefore is a geometric mean, and will be labelled
	\code{geomeanB}.
	Otherwise it is the arithmetic mean and labelled \code{meanB}.
      }
      \item{\code{seB}}{The estimated standard error of the \code{meanB}
	\code{estimate}. If \code{settings$endptscale=="log"} in the
	\code{fit} object, this estimate will be based on the Delta
	method, and will particularly begin to be a poor approximation when the
	standard error in the logscale exceeds 0.50. 
      }
    }	
    An additional column \code{addpct} of percent differences is added if
    \code{endptscale=="original"} and \code{addpct=TRUE},
    as a descriptive supplement to the original scale
    differences that are formally estimated.
    
  }
  \references{
    Hothorn, T., Bretz, F., Westfall, P., Heiberger, R.M., and
    Schuetzenmeister, A. (2010). The \code{multcomp} package.

    Hothorn, T., Bretz, F., and Westfall, P. (2008).
    "Simultaneous Inference in General Parametric Models",
    \emph{Biometrical Journal}, 50, 3, 346-363.  
  }
  \author{
    Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
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


