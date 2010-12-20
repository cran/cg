\name{pointGraph}
\docType{methods}
\alias{pointGraph}
\title{
   Graph Individual Data Points of Groups
}
\description{
  Generic function to create point graphs
  (a.k.a. dot plot, strip plot, one-dimensional scatter plot) 
  of a data object created by the \pkg{cg}
  package.  
}
\usage{
pointGraph(data, \dots) 
}
\arguments{
  \item{data }{
    A data object created with a \code{\link{prepare}} function
    or method from the \pkg{cg}
    package. The only class of object currently
    valid is \code{\link{cgOneFactorData}}, which is created by the
    \code{\link{prepareCGOneFactorData}} function.  
  }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. Currently, there is only one such specific method; see
    \code{\link{pointGraph.cgOneFactorData}} for any additional
    arguments that can be specified.
  }
}
\details{
  Individual points are \code{\link{jitter}}ed, and open circles
  are used to alleviate potential overlap and the danger of representing
  multiple points as a single point.

  The point graph is a vertical dot plot or strip plot, with separate areas
  for each group in the data set, and light gray lines between the groups.
  For censored data, left-censored values are shown as a shallow "V",
  which is actually just a rotated downward "<" sign. Similarly, right-censored
  values are shown as a deeper "^", which is a rotated upward ">" sign.

  Minimum and maximum values from ranges of data are
  respectively labeled in the bottom and top left corners of graph regions.
  
  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.

}
\value{
  The main purpose is the side
  effect of graphing to the current device. See the specific methods for
  discussion of any return values.
}
%% \references{
%% }
\author{
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{pointGraph.cgOneFactorData}}
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
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









