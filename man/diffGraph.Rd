\name{diffGraph}
\docType{methods}
\alias{diffGraph}
\title{
   Graph Paired Differences
}
\description{
  Generic function to create a graph of differences
  from a data object created by the \pkg{cg}
  package.  
}
\usage{
diffGraph(data, \dots)
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
    \code{\link{diffGraph.cgPairedDifferenceData}} for any additional
    arguments that can be specified.
  }
}
\details{
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
  \code{\link{diffGraph.cgPairedDifferenceData}}
}

\examples{
data(anorexia.FT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
diffGraph(anorexiaFT.data) 

# Graph the data on the original scale instead of the log scale.
diffGraph(anorexiaFT.data, logscale=FALSE)
}
\concept{difference graph}









