\name{comparisonsTable.cgPairedDifferenceFit}
\docType{methods}
\alias{comparisonsTable.cgPairedDifferenceFit}
\alias{comparisonsTable,cgPairedDifferenceFit-method}
\alias{cgPairedDifferenceComparisonsTable-class}
\alias{cgPairedDifferenceComparisonsTable}
\title{
  Create a table of comparisons between two groups with the cgPairedDifferenceFit object
}
\description{
  Create a table of comparisons based on the cgPairedDifferenceFit
  object. A cgPairedDifferenceComparisonsTable class object is created.
}
\usage{
\S4method{comparisonsTable}{cgPairedDifferenceFit}(fit, type="pairwisereflect",
 alpha=0.05, addpct=FALSE, display="print",  \dots)
}
\arguments{
  \item{fit}{
    An object of class \code{\link{cgPairedDifferenceFit}}.
  }
  \item{type }{Can be one of two values:

    \describe{
      \item{\code{"pairwisereflect"}}{
	The default value, it calculates and lists both
	pairwise comparison orders.
	In other words, Groups A vs. B and B vs. A will be
	included.
      }
      \item{\code{"pairwise"}}{Calculates and lists only one of the
	two possible pairwise comparison orders.
	Ib other words, once A vs. B is specified, then the reflective B vs. A is not
	included. So the number of comparisons is half that produced
	by \code{"pairwisereflect"}. The ordering of group levels in the
	\code{fit} object is used to determine which
	ordering is included and which is not. If both orderings are
	of interest, such as for \code{settings$endptscale=="log"} in the fit
	objects, use the \code{"pairwisereflect"} value above.
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
	    created \cr\code{cgPairedDifferenceComparisonsTable} object, which is
	    a formatted text output of the table(s).
	  }
	  \item{\code{"none"}}{
	    Supresses any printing. Useful, for example, when just assignment
	    of the resulting object is desired.
	  }
	  \item{\code{"show"}}{
	    Calls the default \code{\link{showDefault}} method, which
	    will just print out the \code{cgPairedDifferenceComparisonsTable} components.
	  }
	}
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid:
    \describe{
      \item{\code{model} }{For \code{cgPairedDifferenceFit} \code{fit} objects that have
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link[MASS]{rlm}} fits, the following argument values are possible:

	\describe{
	  \item{\code{"both"}}{Comparison tables based on both the ordinary
	    classical least squares and resistant & robust fits are
	    created. This is the default when both fits are present in
	    the \code{cgPairedDifferenceFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a comparison table based on the ordinary
	    classical least squares \code{olsfit} fit slot is performed.
	  }
	  \item{\code{"rronly"}}{Only a comparison table based on the
            resistant and robust \code{rrfit} fit slot is performed.
	  }
	}
      }
    }
  }
}
\value{
  Creates an object of class  \code{cgPairedDifferenceComparisonsTable}, with the
  following slots:

    \describe{
      \item{\code{ols.comprs}}{The table of comparisons based on the
	\code{olsfit} component of the \code{\link{cgPairedDifferenceFit}},
	unless \code{model="rronly"} is specified. In that case the slot
	value is \code{NULL}.  See below for the data frame structure
	of the table.
      }
      \item{\code{rr.comprs}}{The table of comparisons based on the
	\code{rrfit} component of the \code{\link{cgPairedDifferenceFit}}
	object, if a valid resistant & robust fit object is present.
	If \code{rrfit} is a simple character value of
	\code{"No fit was selected."}, or \code{model="olsonly"} was
	specified, then the value is \code{NULL}. See below for the data frame structure
	of the table.
      }
      \item{\code{settings}}{A list of settings carried from the
	\code{\link{cgPairedDifferenceFit}} \code{fit} object, and the addition
	of some specified arguments in the method call above:
	\code{alpha},
	\code{type}, and \code{addpct}. These are used
	for the \code{\link{print.cgPairedDifferenceComparisonsTable}} method,
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
    differences that are formally estimated. This is only possible for
    the \code{model=="ols"} case, since the original arithmetic means
    are not estimated in the Resistant & Robust \code{model=="rr"} case.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\examples{
data(anorexiaFT)

## log scale
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)


anorexiaFT.comps <- comparisonsTable(anorexiaFT.fit, display="none")
print(anorexiaFT.comps)

comparisonsTable(anorexiaFT.fit, model="olsonly")
comparisonsTable(anorexiaFT.fit, model="rronly")


## original scale evaluation
anorexiaFT.orig.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                      analysisname="Anorexia FT",
                                                      endptname="Weight",
                                                      endptunits="lbs",
                                                      expunitname="Patient",
                                                      digits=1,
                                                      logscale=FALSE)
anorexiaFT.orig.fit <- fit(anorexiaFT.orig.data)

comparisonsTable(anorexiaFT.orig.fit)

comparisonsTable(anorexiaFT.orig.fit, addpct=TRUE)

}
\concept{comparisons}
