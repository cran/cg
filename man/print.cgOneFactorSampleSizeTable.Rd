\name{print.cgOneFactorSampleSizeTable}
\docType{methods}
\alias{print,cgOneFactorSampleSizeTable-method}
\alias{print.cgOneFactorSampleSizeTable}
\title{
  Print a One Factor Sample Size Table object with some format options
}
\description{
  Print a \code{cgOneFactorSampleSizeTable} object, which contains a table of 
  sample size estimates based on a \code{cgOneFactorFit} 
  object.
}
\usage{
\S4method{print}{cgOneFactorSampleSizeTable}(x, title=NULL, endptname=NULL, \dots)
}
\arguments{
 \item{x}{
   A \code{cgOneFactorSampleSizeTable} object,
   typically created by\cr 
   \code{\link{samplesizeTable.cgOneFactorFit}}.
 }
 \item{title}{
    The title for the table.  If \code{NULL}, it is set to be "Sample Size Table from"
    concatenated to \code{planningname} value
    in the \code{settings} slot of the\cr
    \code{cgOneFactorSampleSizeTable} object.
  }
  \item{endptname}{
    The endpoint name, printed out with the table. 
    If \code{NULL}, it is set to 
    the \code{endptname} value from the \code{settings} slot of the  
    \code{cgOneFactorSampleSizeTable} object.
  }
  \item{\dots }{
    Additional arguments. Currently one is valid: 
    \describe{
      \item{\code{model}}{
	For \code{cgOneFactorComparisonsTable}
	objects that have tables derived from
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:

	\describe{
	  
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant robust sample size
	    tables are printed. This is the default when both fits are present in
	    the \code{cgOneFactorSampleSizeTable} object specified in the \code{x}
	    argument. If only one of the two tables were computed, then
	    only that single table is printed.
	  }
	  \item{\code{"olsonly"}}{Only the ordinary 
	    classical least squares group summary table is printed.
	  }
	  \item{\code{"rronly"}}{Only the resistant & robust
	    group summary table is printed.
	  }
	}
	
      }

    }
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgOneFactorSampleSizeTable}}
  for details of the \code{*.sstable} and other object slots.
}
\value{
  \code{print.cgOneFactorSampleSizeTable} returns \code{\link{invisible}}.
  The main purpose is the side
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
\seealso{
  \code{\link{cgOneFactorSampleSizeTable}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.samplesize <- samplesizeTable(canine.fit, direction="increasing",
                                     model="olsonly",
                                     mmdvec=c(10, 25, 50, 75, 100), display="none")
print(canine.samplesize)
}
