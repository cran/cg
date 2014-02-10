\name{kmGraph}
\docType{methods}
\alias{kmGraph}
\title{
  Graph Distribution Functions of Groups
}
\description{
  Create non-parametric survival or cumulative distribution graphs 
  based on a data object in the \pkg{cg}
  package.  
}
\usage{
kmGraph(data, cgtheme = TRUE, distfcn = "survival", ylab = NULL,
 title = NULL, \dots)
}
\arguments{
  \item{data}{
    A data object created using the \pkg{cg}
    package. The only class of object currently
    available is \code{\link{cgOneFactorData}}, which is created by the
    \code{\link{prepareCGOneFactorData}} function.  
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme, namely \code{background},
    \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
  }
  \item{distfcn}{
    A \code{character}, which may be either \code{"survival"} to graph the
    survival function, or \code{"cumulative"} to graph the cumulative
    distribution function.
  }
  \item{ylab }{
    Specify a character value for the y-axis label. The default value is
    \code{NULL}.
  }
  \item{title }{
    Specify a character value for the main title at the top of the
    graph. The default value is
    \code{NULL}.
  }
  \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. Currently, there is only one such specific method; see
    \code{\link{kmGraph.cgOneFactorData}} for any additional
    arguments that can be specified.
  }
}
\details{Color assignments of the graphed step functions lines for the groups
  match the order of the group name factor levels. The color order is
  given in \code{\link{cgLineColors}} . The line widths are set to be
  thicker (\code{lwd=2}), and the group name label is placed near the line
  using \code{\link{label}} methodology from the \pkg{Hmisc} package.

  The x-axis represents response values, and y-axis represents estimated
  probabilities. Minimum and maximum values from ranges of data are
  respectively labeled in the bottom left and right corners of graph regions.
}
\value{
  The main purpose is the side
  effect of graphing to the current device. See the specific methods for
  discussion of any return values.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
	  \code{\link{kmGraph.cgOneFactorData}}
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
  
