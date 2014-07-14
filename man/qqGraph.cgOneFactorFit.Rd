\name{qqGraph.cgOneFactorFit}
\docType{methods}
\alias{qqGraph,cgOneFactorFit-method}
\alias{qqGraph.cgOneFactorFit}
\title{
  Quantile-Quantile (QQ) Graphs of a cgOneFactorFit object
}
\description{
  Create a Q-Q Gaussian graph of the residuals of a cgOneFactorFit object
}
\usage{
\S4method{qqGraph}{cgOneFactorFit}(fit, line=NULL, cgtheme = TRUE, device = "single", \dots)  
}
\arguments{
  \item{fit }{
    A fit object of class \code{\link{cgOneFactorFit}}. 
  }
  \item{line }{
    Add a line through the estimated 25th and 75th percentiles.
    When set to the default \code{NULL}, the addition
    of a line depends on the following: When there is no censored
    data, the line will be added with the \code{\link{qqline}} algorithm.
    If any censored data residuals are present, no line is added unless
    \code{line=TRUE} is explicitly specified below.
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
      \item{\code{model} }{For \code{cgOneFactorFit} \code{fit} objects that have 
	classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Q-Q graphs of the residuals from both the ordinary 
	    classical least squares and resistant & robust fits are
	    plotted. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
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
	
	For other possible \code{cgOneFactorFit} fit slots such as
	accelerated failure time or unequal variance models, the \code{model}
	argument is not relevant, and the appropriate Q-Q graph will
	be plotted for these model types.
      }

    }
  }
}
\value{
  \code{qqGraph.cgOneFactorFit} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\references{
Gentleman, R.C. and Crowley, J. (1991).  "Graphical Methods for Censored
Data", \emph{Journal of the American Statistical Association}, Volume
86, 678-683.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\details{
  For censored data residuals, left-censored values are shown as a shallow "V",
  which is actually just a rotated downward "<" sign. Similarly, right-censored
  values are shown as a deeper "^", which is a rotated upwared ">" sign.

  For the \code{line} argument, an added line when censored data residuals
  are present needs to be interpreted very cautiously. If "too many"
  censored data values are present, the line will appear nonsensical if
  indeed it can even be estimated with 25th and 75th percentiles in the
  presence of the the censored data residuals. These percentiles are
  estimated via the Kaplan-Meier method as proposed by Gentleman and Crowley
  (1991), with the \pkg{survival}::\code{survfit} function.

  The heading for the graph is taken from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{analysisname} 
  argument.

  The label for the Y-axis of residuals is derived from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{endptname} 
  argument.

  The number of decimal places printed in the ticks on the Y-axis is taken
  from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{digits} 
  argument.

  The minimum and maximum values from the range of the residuals are
  respectively labeled in the bottom and top left corners of the graph region.
}
\seealso{
  \code{\link{qqline}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

qqGraph(canine.fit)

qqGraph(canine.fit, model="olsonly")

}

\concept{quantile-quantile}
\concept{residual diagnostics}

