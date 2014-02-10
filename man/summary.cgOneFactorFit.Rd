\name{summary.cgOneFactorFit}
\docType{methods}
\alias{summary,cgOneFactorFit-method}
\alias{summary.cgOneFactorFit}
\title{
  Summary display of a One Factor Model Fit object with some format options 
}
\description{
  Summary printing of a \code{cgOneFactorFit} object,
  which contains fitted model information.
}
\usage{
\S4method{summary}{cgOneFactorFit}(object, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{object }{
    An \code{\link{cgOneFactorFit}} object.
  }
  \item{title }{
    The title printed out with the summary of the fitted model(s).
    If \code{NULL}, it is set to be "Fitted Model Summaries of"
    the \code{analysisname} value in the \code{settings} slot of the 
    \code{\link{cgOneFactorFit}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the fitted model information. 
    If \code{NULL}, it is set to 
    the \code{endptname} value in the \code{settings} slot of the 
    \code{\link{cgOneFactorFit}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorFit}
	objects that have output derived from
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant & robust model fit
	    summaries are printed.
	    This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{object}
	    argument.
	  }
	  \item{\code{"olsonly"}}{Only the ordinary 
	    classical least squares model fit summary is printed.
	  }
	  \item{\code{"rronly"}}{Only the resistant &
	    robust model fit summary is printed.
	  }
	}
      }
    }
    For other possible \code{cgOneFactorFit} 
    components such as
    accelerated failure time or unequal variance model fits, the \code{model}
    argument is not relevant, and the single model fit summary is printed
    for these model types.
  }
  
}
\details{
  The object summary is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgOneFactorFit}}
  for details of the \code{*fit} and other object slots.

  This method simply echoes summary methods for individual fit classes,
  such as \code{\link{lm}} and \code{\link{rlm}}.
}
\value{
  \code{summary.cgOneFactorFit} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
  typically the console.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{cgOneFactorFit}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

summary(canine.fit)


}

