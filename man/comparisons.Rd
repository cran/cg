\name{comparisons}

\alias{comparisons}
\title{
  Create a table of comparisons amongst groups
}
\description{
  Create a table of comparisons based on group estimates and
  variance-covariance matrix. Pairwise or custom specified contrasts are estimated and
  tested.  
}
\usage{
comparisons(estimates, varcovmatrix, errordf = Inf, endptscale,
            mcadjust = FALSE, alpha = 0.05, type = "pairwisereflect",
            contrastmatrix = NULL, n, offset = NULL, cnames = "derive",
            analysisname = "", endptname = "", digits = NULL, addpct = FALSE,
            display = "print", \dots) 
}
\arguments{
  \item{estimates }{
    A named vector of estimates. Each estimate element is a measure of
    the center of the group. The name of each group must
    be present in the names attribute of the vector.
  }
  \item{varcovmatrix }{
    The estimated variance-covariance matrix associated with the
    \code{estimates}. Must have the same number of columns and rows as the length
    of the \code{estimates} vector.
  }
  \item{errordf }{Can be one of three types of values:

    \describe{
      \item{\code{Inf}}{The default, and will just use standard Gaussian
	(normal) distribution quantile for the critical points in each
	comparison;
      }
      \item{\code{numeric}}{A finite positive number that will be used for
	the degrees of freedom for the t-distribution quantile;
      }
      \item{\code{"approx"}}{Will try to apply a Satterthwaite
	approximation based on the \code{variance-covariance} matrix and
	\code{n} to
	estimate the degrees of freedom for each comparison. The
	variance-covariance matrix will need to be diagonal. Only will be
	accepted when \code{mcadjust=FALSE}.
      }
    }
    
  }
  \item{endptscale }{Must be specified as \code{"log"} or
    \code{"original"}. If \code{"log"} then the \code{estimates} vector
    is assumed to be in the log scale, and calculations will need to
    transform to the original scale for the generated table.
  }
  \item{mcadjust }{
    Do a multiple comparisons adjustment, based on the simultaneous
    inference capabilities of the \pkg{multcomp} package. See Details
    below. The default value is \code{FALSE}.
    If \code{mcadjust=TRUE} is specified, there will be a delay,
    usually just for a few seconds, due to computing time of the
    critical point in order to conduct the adjusted comparisons.
  }
  \item{alpha }{Significance level, by default set to \code{0.05} to
    correspond to 95\% confidence. 
  }
  \item{type }{Can be one of four values:

    \describe{
      \item{\code{"pairwisereflect"}}{
	The default value, it calculates and lists all possible
	pairwise comparison permutations, as each pair ordering is
	included. In other words, Groups A vs. B and B vs. A will be
	included.
      }
      \item{\code{"pairwise"}}{Calculates and lists all possible
	pairwise comparison combinations. Once a pair such as Groups
	A vs. B is specified, then the reflective B vs. A is not
	included. So the number of comparisons presented is half that produced
	by \code{"pairwisereflect"}. The ordering of group levels in the
	\code{estimates} vector is used to determine which
	ordering is included and which is not. If all orderings are
	of interest, such as for \code{endptscale=="log"},
	use the \code{"pairwisereflect"} value above.
      }
      \item{\code{"allgroupstocontrol"}}{Takes the first element
	group of the \code{estimate} vector,
	deems it the "control" group, and constructs
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
    of columns must be equal to the number of groups in the
    \code{estimate} vector. Each row in
    the matrix is assumed to represent a contrast of coefficients
    amongst the groups that defines the comparison of interest.
  }
  \item{n }{Needs to be specified only when
    \code{errordf="approx"}. In this case it needs to be a vector of
    group sample sizes, the same in length as the \code{estimates} vector.
  }
  \item{offset }{\emph{Optional}, 
    If for example a numeric constant was added to all response values
    before calculation of the \code{estimates} as means in the
    \code{"log"} scale, this could be used to
    adjust the estimates and comparisons appropriately. The default
    \code{NULL}.
  }
  \item{cnames }{
    If the default value of \code{derive} is used, row names are
    derived for the table that reflect the A vs. B type of
    comparison items in each row, using the names attrributes of the
    \code{estimates} vector and the middle term of "vs." Otherwise,
    this can be explicitly specified and needs to be a character vector of
    the same length as \code{estimates}
  }
  \item{analysisname }{\emph{Optional}, a character text that will be
    printed along with the results table. The default
    value is the empty \code{""}.}
  \item{endptname }{\emph{Optional}, a character text that will be
    printed along with the results table. The default
    value is the empty \code{""}.}
  \item{digits }{\emph{Optional}, For output display purposes,
    values will be rounded to this numeric
    value. Only the integers of 0, 1, 2, 3, and 4 are accepted. No
    rounding is done during any calculations. The default value is
    \code{NULL}, which will examine each individual \code{estimates}
    value and choose the
    one that has the maximum number of digits after any trailing
    zeroes are ignored. The max number of digits will be 4.}
  \item{addpct }{Only relevant if \code{endptscale=="original"}.
    An column of percent differences is added for the
    comparisons, as a descriptive supplement to the original scale
    differences that are formally estimated.
  }
  \item{display }{One of three valid values:
    
    \describe{
      \item{\code{"print"}}{
	The default value, it calls a \code{print} method for the
	created object, which is
	a formatted text output of the table(s).
      }
      \item{\code{"none"}}{
	Supresses any printing. Useful, for example, when just assignment
	of the resulting object is desired.
      }
      \item{\code{"show"}}{
	Calls the default \code{\link{showDefault}} method, which
	will just print out the \code{comparisons} return object.
      }
    }
  }
  \item{\dots }{
    Additional arguments. None are currently used.
  }
}
\section{Warning}{
  This function was created for internal use in the \pkg{cg} package as
  its use can be seen in the \code{\link{comparisonsTable}} methods
  code. Therefore any direct use of it needs to be done cautiously.
}
\details{
  Only two-sided Wald-type of confidence intervals are possible with
  this function. 
  
  When \code{mcadjust=TRUE}, a status message of \code{"Some time may be needed as the critical point from the multcomp::summary.glht function call is calculated"} is displayed at the console. This computed critical point
  is used for all subsequent p-value and confidence interval
  calculations.

  The \pkg{multcomp} package provides a unified way to calculate
  critical points based on the comparisons of interest in a
  "family." Thus a user does not need to worry about choosing amongst
  the myriad names of multiple comparison procedures.
}
\value{
  Creates a return data frame object that specifies the comparison of the
  form A vs. B in each row, and with these columns:

  \describe{
    \item{\code{estimate}}{The difference in group estimates in the
      comparison: A vs. B. If \code{endptscale="log"},
      this will back-transformed to a percent
      difference scale.
    }
    \item{\code{se}}{The estimated standard error of the difference
      \code{estimate}. If \code{endptscale="log"},
      this estimate will be based on the Delta
      method, and will particularly begin to be a poor approximation when the
      standard error in the logscale exceeds 0.50. 
    }
    \item{\code{lowerci}}{The lower 100 * (1-\code{alpha}) \% confidence limit of the
      difference \code{estimate}. With the default \code{alpha=0.05},
      this is 95\%. If \code{endptscale="log"},
      the confidence limit is first computed in the
      logarithmic scale of analysis, and then back-transformed to a percent
      difference scale.
    }
    \item{\code{upperci}}{The upper 100 * (1-\code{alpha}) \% confidence limit of the
      difference \code{estimate}. With the default \code{alpha=0.05},
      this is 95\%. If \code{endptscale="log"},
      the confidence limit is first computed in the
      logarithmic scale of analysis, and then back-transformed to a percent
      difference scale.
    }
    \item{\code{pval}}{The computed p-value from the test of the difference \code{estimate}.
    }
    \item{\code{meanA}}{The estimated "mean" for the left hand side "A" of the A
      vs. B  comparison. If \code{endptscale="log"},
      this is a back-transform to the original scale,
      and therefore is a "geometric" mean.
    }
    \item{\code{seA}}{The estimated standard error of the \code{meanA}
      \code{estimate}. If \code{endptscale="log"}, this estimate will be based on the Delta
      method, and will particularly begin to be a poor approximation when the
      standard error in the logscale exceeds 0.50. 
    }
    \item{\code{meanB}}{The estimated "mean" for the right hand side "B" of the A
      vs. B comparison. If \code{endptscale="log"}, this is a back-transform to the original scale,
      and therefore is a "geometric" mean.

    }
    \item{\code{seB}}{The estimated standard error of the \code{meanB}
      \code{estimate}. If \code{endptscale="log"}, this estimate will be based on the Delta
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
  Schuetzenmeister, A. (2010). The \pkg{multcomp} package.

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


## Easier way: notice the comparisonsTable call

comparisonsTable(canine.fit, model="olsonly")

## Manual way
## Instead of comparisonsTable(canine.fit, model="olsonly")

comparisons(estimates=canine.fit@olsfit$coef,
            varcovmatrix=vcov(canine.fit@olsfit),
            errordf=canine.fit@olsfit$df.residual,
            endptscale="log",
            analysisname="Canine",
            digits=1,
            endptname="Prostate Volume")
}

\concept{comparisons}
\concept{multiplicity}


