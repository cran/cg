\name{pointGraph.cgOneFactorData}
\docType{methods}
\alias{pointGraph.cgOneFactorData}
\alias{pointGraph,cgOneFactorData-method}
\title{
  Graph Individual Data Points in a cgOneFactorData object
}
\description{
  Create a point graph (a.k.a. dot plot, strip plot, one-dimensional
  scatter plot) of the data in a \code{cgOneFactorData} object.
}
\usage{
\S4method{pointGraph}{cgOneFactorData}(data, \dots)
}
\arguments{
  \item{data}{
    A \code{\link{cgOneFactorData}} object.
  }
  \item{\dots }{
    Additional arguments, both \emph{optional}. Two are currently valid: 
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the point graph should be
	plotted on the logarithmic scale.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgOneFactorData} object,
	which \code{\link{prepareCGOneFactorData}} sets from its \code{logscale} argument.
      }
      \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the y-axis,
		and create the tickmarks specified in the
		\code{marks} component below.
	      }
	      \item{\code{"add"}}{Before graphing the data, add
		tickmarks specified in the 
		\code{marks} component below, to the automatically
		generated ones.
	      }
	    }
	  }
	  \item{\code{marks}}{A vector of tickmarks to be
	    placed on the y-axis. Any numeric representations will
	    be coerced to character.
	  }
	}
      }
    }
  }
}
\details{
  If \code{logscale=TRUE}, the tick marks for the y-axis
  on the left side of the plot show original values, while the
  tick marks for the y-axis on the right side of the graph 
  show base 10 log values.

  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.
  
  Individual points are \code{\link{jitter}}ed, and open circles
  are used to alleviate potential overlap and the danger of representing
  multiple points as a single point.

  The point graph is a vertical dot plot or strip plot, with separate areas
  for each group in the data set, and light gray lines between the groups.
  For censored data, left-censored values are shown as a shallow "V",
  which is actually just a rotated downward "<" sign. Similarly, right-censored
  values are shown as a deeper "^", which is a rotated upward ">" sign.

  The heading for the graph is taken from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{analysisname} 
  argument.

  The label for the y-axis is taken from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{endptname} 
  and \code{endptunits} arguments.

  The number of decimal places printed in the ticks on the y-axis is taken
  from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{digits} 
  argument.

  The minimum and maximum values from the range of the data are
  respectively labeled in the bottom and top left corners of the graph
  region.
  
  If group labels along the x-axis seem to overlap in the standard
  horizontal form, they will be rotated 45 degrees.
}
\value{
  \code{pointGraph.cgOneFactorData} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device. 
}
%%\references{
%%}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
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
pointGraph(canine.data)

# Graph the data on the original scale instead of the log scale.
pointGraph(canine.data, logscale=FALSE)

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
pointGraph(gmcsfcens.data)
}

\concept{point graph}
\concept{dot plot}
\concept{strip plot}


