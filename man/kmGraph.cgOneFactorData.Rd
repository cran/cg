\name{kmGraph.cgOneFactorData}
\docType{methods}
\alias{kmGraph,cgOneFactorData-method}
\alias{kmGraph.cgOneFactorData}
\title{
  Graph Distribution Functions of Groups in a cgOneFactorData object
}
\description{Create a non-parametric survival or cumulative distribution graph
  of groups of data in a \code{cgOneFactorData} object.
}
\usage{
\S4method{kmGraph}{cgOneFactorData}(data, cgtheme = TRUE, distfcn ="survival",
 ylab = NULL, title = NULL, \dots)
}
\arguments{
  \item{data}{
    A \code{cgOneFactorData} object.
  }
  \item{cgtheme}{
    A \code{logical} value, indicating whether to use the cg limited color scheme (white
    backgrounds, including shingles) or the current Trellis
    color scheme. The default is \code{TRUE}.
  }
  \item{distfcn}{
    A \code{character} value, which may be either the default \code{"survival"} to graph the
    survival function, or \code{"cumulative"} to graph the cumulative
    distribution function.
  }
  \item{ylab}{
    \emph{Optional}, a \code{character} value to label the y-axis of the graph.  If 
    \code{NULL}, it
    defaults to 
    either \code{"Probability of Survival"}\cr
    or \code{"Cumulative Probability"},
    depending on the value of \code{distfcn}.
  }
  \item{title}{
    \emph{Optional}, a \code{character} to show as the title of the graph.  If
    \code{NULL},
    it defaults to 
    either \code{"Nonparametric Survival Curve Estimates"}\cr or
    \code{"Nonparametric Cumulative Distribution Function Estimates"},
    depending on the value of \code{distfcn}.
  }
  \item{\dots}{
    Additional arguments. One is currently valid:
	
    \describe{
	
	  \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the x-axis,
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
	    placed on the x-axis. Any numeric representations will
    be coerced to character.
	  }
	}
      }
    }
	
	
  }
}
\details{
  Graph the estimated survival function or cumulative distribution for
  each group in a
  \code{cgOneFactorData} object.  For censored data, Kaplan-Meier estimates 
  are used. For uncensored data, the conventional step function empirical
  estimates are used.

  Color assignments of the graphed step functions lines for the groups
  match the order of the group name factor levels. The color order is
  given in \code{\link{cgLineColors}}. The line widths are set to be
  thicker (\code{lwd=2}), and the group name label is placed near the line
  using \code{\link{label}} methodology from the \pkg{Hmisc} package.

  The x-axis represents response values, and y-axis represents estimated
  probabilities. Minimum and maximum values from ranges of data are
  respectively labeled in the bottom left and right corners of graph regions.

  The label for the x-axis is taken from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its
  \code{endptname} and \code{endptunits} arguments.

  The number of decimal places printed in the ticks on the x-axis is taken
  from the \code{cgOneFactorData} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{digits} 
  argument.
}
\value{
  \code{kmGraph.cgOneFactorFit} returns
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
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
kmGraph(canine.data, distfcn="cumulative")
kmGraph(canine.data, distfcn="cumulative",
        ticklabels=list(mod="add", marks=c(2)))

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
kmGraph(gmcsfcens.data, distfcn="cumulative")
kmGraph(gmcsfcens.data, distfcn="cumulative", logscale=FALSE)
}

\concept{survival curve}
\concept{Kaplan-Meier}
\concept{empirical distribution function}