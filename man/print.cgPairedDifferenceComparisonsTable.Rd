\name{print.cgPairedDifferenceComparisonsTable}
\docType{methods}
\alias{print.cgPairedDifferenceComparisonsTable}
\alias{print,cgPairedDifferenceComparisonsTable-method}
\title{
  Print Paired Difference Comparisons Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceComparisonsTable} object, which contains a
  table of comparisons based on the \code{cgPairedDifferenceFit}
  object.
}
\usage{
\S4method{print}{cgPairedDifferenceComparisonsTable}(x, digits = NULL, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    An \code{\link{cgPairedDifferenceComparisonsTable}} object,
    typically created by\cr \code{\link{comparisonsTable.cgPairedDifferenceFit}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.
	If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the\cr
    \code{\link{cgPairedDifferenceComparisonsTable}} object.
  }
  \item{title }{
    The title printed out with the table.  If \code{NULL},
    it is set to be "Comparisons Table of"
    the \code{analysisname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceComparisonsTable} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the table.
    If \code{NULL}, it is set to
    the \code{endptname} value from the \code{settings} slot of the
    \code{\link{cgPairedDifferenceComparisonsTable}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid:
    \describe{
      \item{\code{model}}{
	  For \code{cgPairedDifferenceComparisonsTable}
	  objects that have tables derived from
	  classical least squares \code{\link{lm}} or resistant & robust
	\code{\link[MASS]{rlm}} fits, the following argument values are possible:

	\describe{

	  \item{\code{"both"}}{Both the ordinary
	    classical least squares and resistant & robust comparisons
	    tables are printed. This is the default when both fits are present in
	    the \code{cgPairedDifferenceComparisonsTable} object specified in the \code{x}
	    argument.
	  }
	  \item{\code{"olsonly"}}{Only the ordinary
	    classical least squares group comparisons table is printed.
	  }
	  \item{\code{"rronly"}}{Only the resistant & robust
	    comparisons table is printed.
	  }
	}
      }
    }
   }
}

\details{
  The smallest actual p-value that will be printed is \code{0.001}. Anything
  less than \code{0.001} will be displayed as \code{< 0.001}. If you
  need more digits, see the \code{\link{cgPairedDifferenceComparisonsTable}} object.

  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgPairedDifferenceComparisonsTable}}
  for details of the \code{*.comprs} and other object slots.
}
\value{
  \code{print.cgPairedDifferenceComparisonsTable} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
  typically the console.
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

anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)

anorexiaFT.fit <- fit(anorexiaFT.data)

anorexiaFT.comps <- comparisonsTable(anorexiaFT.fit)

print(anorexiaFT.comps, digits=2)

print(anorexiaFT.comps, model="olsonly")

}

