\name{summary.cgPairedDifferenceFit}
\docType{methods}
\alias{summary,cgPairedDifferenceFit-method}
\alias{summary.cgPairedDifferenceFit}
\title{
  Summary display of a Paired Difference  Model Fit object with some format options 
}
\description{
  Summary printing of a \code{cgPairedDifferenceFit} object,
  which contains fitted model information.
}
\usage{
\S4method{summary}{cgPairedDifferenceFit}(object, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{object }{
    An \code{\link{cgPairedDifferenceFit}} object.
  }
  \item{title }{
    The title printed out with the summary of the fitted model(s).
    If \code{NULL}, it is set to be "Fitted Model Summaries of"
    the \code{analysisname} value in the \code{settings} slot of the 
    \code{\link{cgPairedDifferenceFit}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the fitted model information. 
    If \code{NULL}, it is set to 
    the \code{endptname} value in the \code{settings} slot of the 
    \code{\link{cgPairedDifferenceFit}} object.
  }
  \item{\dots }{
    Additional arguments. Only one is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgPairedDifferenceFit}
	objects that have output derived from
	classical least squares \code{\link{lm}} or resistant & robust
	\code{\link{rlm}} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Both the ordinary 
	    classical least squares and resistant & robust model fit
	    summaries are printed.
	    This is the default when both fits are present in
	    the \code{cgPairedDifferenceFit} object specified in the \code{object}
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
  }  
}
\details{
  The object summary is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgPairedDifferenceFit}}
  for details of the \code{*fit} and other object slots.

  This method simply echoes summary methods for individual fit classes,
  such as \code{\link{lm}} and \code{\link{rlm}}.
}
\value{
  \code{summary.cgPairedDifferenceFit} returns
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
\seealso{
  \code{\link{cgPairedDifferenceFit}}
}
\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)

summary(anorexiaFT.fit)
}

