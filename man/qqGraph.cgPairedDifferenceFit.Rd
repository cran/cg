\name{qqGraph.cgPairedDifferenceFit}
\docType{methods}
\alias{qqGraph,cgPairedDifferenceFit-method}
\alias{qqGraph.cgPairedDifferenceFit}
\title{
  Quantile-Quantile (QQ) Graphs of a cgPairedDifferenceFit object
}
\description{
  Create a Q-Q Gaussian graph of the residuals of a cgPairedDifferenceFit object
}
\usage{
\S4method{qqGraph}{cgPairedDifferenceFit}(fit, line=TRUE, cgtheme = TRUE, device = "single", \dots)  
}
\arguments{
  \item{fit }{
    A fit object of class \code{\link{cgPairedDifferenceFit}}. 
  }
  \item{line }{
    Add a line through the estimated 25th and 75th percentiles,
    when set to the default \code{TRUE}.
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme. Namely, \code{background},
    \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
  }
  \item{device }{Can be one of three values:
    
    \describe{
      \item{\code{"single"}}{The default, which will put all graphs on the same
	device page. For example, when resistant & robust and classical
	least squares are present and \code{model="both"} (the default),
	a 2 x 1 paneled graph will be created.}
      \item{\code{"multiple"}}{Relevant only when resistant & robust and classical
	least squares are present and \code{model="both"} (the default) or 
	\code{model="extended"}. In
	those cases, a new graphics device is generated to hold the resistant & robust
	version, as a single-paneled graph, 
	and the classical least squares version is on the previous device.
	If \code{model="extended"}, then
	a second new graphics device is generated to hold the unweighted
	resistant & robust residuals, as another single-paneled graph.}
      \item{\code{"ask"}}{Relevant only when resistant & robust and classical
	least squares are present and \code{model="both"} (the default) or
	\code{model="extended"}. In
	that case, each are portrayed as a single-paneled graph, with the
	\code{ask=TRUE} argument specified in \code{\link{par}} so that
	the user input confirmation is needed before the graphs are
	drawn.
      }
    }

  }
  \item{\dots }{
    Additional arguments. One is currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgPairedDifferenceFit} \code{fit} objects that have 
	classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Q-Q graphs of the residuals from both the ordinary 
	    classical least squares and resistant & robust fits are
	    plotted. This is the default when both fits are present in
	    the \code{cgPairedDifferenceFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a Q-Q graph of the residuals from
	    the ordinary 
	    classical least squares \code{olsfit} fit is performed.
	  }
	  \item{\code{"rronly"} or \code{"rrwtdonly"}}{Only a Q-Q graph of the 
	    weighted residuals from 
	    the resistant and
	    robust \code{rrfit} fit is plotted.
	  }
	  \item{\code{"rrunwtdonly"}}{Only a Q-Q graph of the unweighted 
	    residuals from 
	    the resistant and
	    robust \code{rrfit} fit is plotted.
	  }
	}
      }

    }
  }
}
\value{
  \code{qqGraph.cgPairedDifferenceFit} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\details{
  The heading for the graph is taken from the \code{cgPairedDifferenceData} object,
  which\cr \code{\link{prepareCGPairedDifferenceData}} sets from its \code{analysisname} 
  argument.

  The label for the Y-axis is taken from the \code{cgPairedDifferenceData} object,
  which\cr \code{\link{prepareCGPairedDifferenceData}} sets from its \code{endptname} 
  argument.

  The number of decimal places printed in the ticks on the Y-axis is taken
  from the \code{cgPairedDifferenceData} object,
  which \code{\link{prepareCGPairedDifferenceData}} sets from its \code{digits} 
  argument.

  The minimum and maximum values from the range of the residuals are
  respectively labeled in the bottom and top left corners of the graph region.
}
\seealso{
  \code{\link{qqline}}
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

qqGraph(anorexiaFT.fit)

qqGraph(anorexiaFT.fit, model="olsonly")

}

\concept{quantile-quantile}
\concept{residual diagnostics}

