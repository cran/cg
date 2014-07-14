\name{profileGraph}
\docType{methods}
\alias{profileGraph}
\title{
   Graph Profiles of Experimental Units
}
\description{
  Generic function to create a graph of experimental unit profiles
  of a data object created by the \pkg{cg}
  package.  
}
\usage{
profileGraph(data, \dots) 
}
\arguments{
  \item{data }{
    A data object created with a \code{\link{prepare}} function
    or method from the \pkg{cg}
    package. The only class of object currently
    valid is \code{\link{cgPairedDifferenceData}}, which is created by the
    \code{\link{prepareCGPairedDifferenceData}} function.  
  }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object. Currently, there is only one such specific method; see
    \code{\link{profileGraph.cgPairedDifferenceData}} for any additional
    arguments that can be specified.
  }
}
\details{
  Individual points are \code{\link{jitter}}ed, and open circles
  are used to alleviate potential overlap and the danger of representing
  multiple points as a single point.

  The profile graph for paired difference data is the simplest of profiles
  as each experimental unit has exactly two points connected by a
  straight line. Labels for the experimental units are added for identification.

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
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{profileGraph.cgPairedDifferenceData}}
}

\examples{
data(anorexia.FT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
profileGraph(anorexiaFT.data) 

# Graph the data on the original scale instead of the log scale.
profileGraph(anorexiaFT.data, logscale=FALSE)
}
\concept{profile graph}









