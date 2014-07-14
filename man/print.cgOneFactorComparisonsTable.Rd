\name{print.cgOneFactorComparisonsTable}
\docType{methods}
\alias{print.cgOneFactorComparisonsTable}
\alias{print,cgOneFactorComparisonsTable-method}
\title{
  Print One Factor Comparisons Table object with some format options
}
\description{
  Print a \code{cgOneFactorComparisonsTable} object, which contains a
  table of comparisons based on the \code{cgOneFactorFit} 
  object.
}
\usage{
\S4method{print}{cgOneFactorComparisonsTable}(x, digits = NULL, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    An \code{\link{cgOneFactorComparisonsTable}} object,
    typically created by\cr \code{\link{comparisonsTable.cgOneFactorFit}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the\cr 
    \code{\link{cgOneFactorComparisonsTable}} object. 
  }
  \item{title }{
    The title printed out with the table.
    If \code{NULL}, it is set to be "Comparisons Table of"
    the \code{analysisname} value from the \code{settings} slot of the 
    \cr\code{\link{cgOneFactorComparisonsTable}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the table. 
    If \code{NULL}, it is set to 
    the \code{endptname} value from the \code{settings} slot of the  
    \code{\link{cgOneFactorComparisonsTable}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model}}{
	  For \code{cgOneFactorComparisonsTable}
	  objects that have tables derived from
	  classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant robust comparisons
	    tables are printed. This is the default when both fits are present in
	    the \code{cgOneFactorComparisonsTable} object specified in the \code{x}
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
    For other possible \code{cgOneFactorComparisonsTable} table
    components such as
    accelerated failure time or unequal variance models, the \code{model}
    argument is not relevant, and the single table will just be printed
    for these model types.
  }
   
}
\details{
  The smallest actual p-value that will be printed is \code{0.001}. Anything
  less than \code{0.001} will be displayed as \code{< 0.001}. If you
  need more digits, see the \code{\link{cgOneFactorComparisonsTable}} object.

  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgOneFactorComparisonsTable}}
  for details of the \code{*.comprs} and other object slots.
}
\value{
  \code{print.cgOneFactorComparisonsTable} returns
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
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")

canine.fit <- fit(canine.data)

canine.comps0 <- comparisonsTable(canine.fit)

print(canine.comps0, digits=1)

canine.comps1 <- comparisonsTable(canine.fit,  mcadjust=TRUE,
                                  type="allgroupstocontrol", refgrp="CC")

print(canine.comps1, model="olsonly")

}

