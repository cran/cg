\name{varianceGraph.cgOneFactorFit}
\docType{methods}
\alias{varianceGraph,cgOneFactorFit-method}
\alias{varianceGraph.cgOneFactorFit}
\title{
  Equal Variance Graphs assessment of cgOneFactorFit object
}
\description{
  Graph residuals of a cgOneFactorFit object to assess equal variance assumption
}
\usage{
\S4method{varianceGraph}{cgOneFactorFit}(fit, trend = NULL, cgtheme = TRUE,
 device = "single", \dots)  
}
\arguments{
  \item{fit }{
    A fit object of class \code{\link{cgOneFactorFit}}. 
  }
  \item{trend }{
    Add a trend line. When set to the default \code{NULL}, the addition
    of a trend line depends on the following: When there is no censored
    data,  a trend will be added with the \code{\link{lowess}} smoother.
    If any censored data residuals are present,
    then at least 7 groups are needed in the fit
    for a trend line to be added with the \pkg{VGAM}::\code{vgam} smoothing
    spline.
    Otherwise, group means are estimated
    and connected. The values of \code{FALSE} and \code{TRUE} override
    this. See Details section below.
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme. Namely \code{background},
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
    Additional arguments. Two are currently valid: 
    \describe{
      \item{\code{model} }{For \code{cgOneFactorFit} \code{fit} objects that have 
	classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Graphs of the residuals from both the ordinary 
	    classical least squares and resistant & robust fits are
	    plotted. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only a graph of the residuals from
	    the ordinary 
	    classical least squares \code{olsfit} fit is performed.
	  }
	  \item{\code{"rronly"} or \code{"rrwtdonly"}}{Only a graph of the 
	    weighted residuals from 
	    the resistant and
	    robust \code{rrfit} fit is plotted.
	  }
	  \item{\code{"rrunwtdonly"}}{Only a graph of the unweighted 
	    residuals from 
	    the resistant and
	    robust \code{rrfit} fit is plotted.
	  }
	}
	
	For other possible \code{cgOneFactorFit} fit slots such as
	accelerated failure time or unequal variance models, the \code{model}
	argument is not relevant, and the appropriate graph will
	be plotted for these model types.
      }

    }
  }
}
\details{
  The graph plots the square root of the absolute value of the residuals against
  the fitted value. The square root spacing on the y-axis has tick marks
  in the fitted scale.  The notion of using the squared root of the absolute residuals is
  attributed to John Tukey.


  The values are automatically jittered to minimize overlapping points.
  For censored data, left-censored values are shown as a shallow "V",
  which is actually just a rotated downward "<" sign. Similarly, right-censored
  values are shown as a deeper "^", which is a rotated upward ">" sign.

  For the \code{trend} argument, an added trend line when censored data residuals
  are present needs to be interpreted cautiously. When there are 7 or more
  groups, a cubic smoothing spline based on \pkg{VGAM}::\code{\link{vgam}}
  is fit; otherwise, the group means of the residuals are estimated with
  an accelerated failure time model and then just connected. If "too many"
  censored data values are present, the line may be withheld and
  warnings will be issued, or if forced with \code{line=TRUE}, for
  example, may appear nonsensical.

  The heading for the graph is taken from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{analysisname} 
  argument.

  The label for the Y-axis is taken from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{endptname} 
  argument.

  The number of decimal places printed in the ticks on the Y-axis is taken
  from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{digits} 
  argument.

  The minimum and maximum values from the range of the absolute valued residuals are
  respectively labeled in the bottom and top left corners of the graph
  region.
    
  If group labels along the x-axis seem to overlap in the standard
  horizontal form, they will be rotated 45 degrees.
}
\value{
  \code{varianceGraph.cgOneFactorFit} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
%% \references{
%% }
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}

%% \seealso{
%% }
\examples{
## From running examples of cgOneFactorData objects
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

varianceGraph(canine.fit)

varianceGraph(canine.fit, model="olsonly")


gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE,
                                         digits=1)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

varianceGraph(gmcsfcens.fit, trend=TRUE)

varianceGraph(gmcsfcens.fit) ## will yield a warning message why no line
                             ## is graphed

varianceGraph(gmcsfcens.fit, trend=FALSE)





}

\concept{equal variance}
\concept{residual diagnostics}

