\name{print.cgOneFactorGrpSummaryTable}
\docType{methods}
\alias{print,cgOneFactorGrpSummaryTable-method}
\alias{print.cgOneFactorGrpSummaryTable}
\title{
  Print One Factor Group Summary Table object with some format options
}
\description{
  Print a \code{cgOneFactorGrpSummaryTable} object, which contains a
  table of group means and variability based on the \code{cgOneFactorFit}
  object.
}
\usage{
\S4method{print}{cgOneFactorGrpSummaryTable}(x, digits = NULL, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{x }{
    An \code{\link{cgOneFactorGrpSummaryTable}} object,
    typically created by\cr\code{\link{grpSummaryTable.cgOneFactorFit}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the \code{\link{cgOneFactorGrpSummaryTable}} object. 
  }
  \item{title }{
    The title printed out with the table.
    If \code{NULL}, it is set to be "Group Summary Table of"
    the \code{analysisname} value from the \code{settings} slot of the 
    \code{\link{cgOneFactorGrpSummaryTable}} object.
  }	
  \item{endptname }{
    The endpoint name, printed out with the table. 
    If \code{NULL}, it is set to 
    the \code{endptname} value from the \code{settings} slot of the  
    \code{\link{cgOneFactorGrpSummaryTable}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorGrpSummaryTable}
	objects that have tables derived from
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant robust comparisons
	    tables are printed. This is the default when both fits are present in
	    the \code{cgOneFactorGrpSummaryTable} object specified in the \code{x}
	    argument.
	  }
	  \item{\code{"olsonly"}}{Only the ordinary 
	    classical least squares comparisons table is printed.
	  }
	  \item{\code{"rronly"}}{Only the resistant and
	    robust comparisons table is printed.
	  }
	}
      }
    }
    For other possible \code{cgOneFactorGrpSummaryTable} table
    components such as
    accelerated failure time or unequal variance models, the \code{model}
    argument is not relevant, and the single table will just be printed
    for these model types.
 }
}
\value{
  \code{print.cgOneFactorGrpSummaryTable} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
  typically the console.
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgOneFactorGrpSummaryTable}}
  for details of the \code{*.grps} and other object slots.
}
\author{
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{cgOneFactorGrpSummaryTable}}
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

print(canine.grpsumm, digits=2)
}


