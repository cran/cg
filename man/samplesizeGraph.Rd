\name{samplesizeGraph}
\docType{methods}
\alias{samplesizeGraph}
\title{
  Graph Estimated Sample Sizes
}
\description{
  Generic function to graph a table of estimated sample sizes, using
  a Sample Size table created by the \pkg{cg} package.  
}
\usage{
samplesizeGraph(sstable, Nscale = "log", mmdscale = "log", \dots)
}
\arguments{
  \item{sstable }{
    A \code{samplesizeTable} created by a \code{samplesizeTable}
    method from the \pkg{cg} package. 
  }
  \item{Nscale }{
    A \code{character} indicating whether the y-axis, which shows the estimated
    sample sizes, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{mmdscale }{
    A \code{character} indicating whether the x-axis, which shows the minimum
    meaningful differences to be detected, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object.  See the specific methods for additional
    details.
  }
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
  \code{\link{samplesizeGraph.cgOneFactorSampleSizeTable}}
}
\examples{
#### One Factor data

data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.samplesize <- samplesizeTable(canine.fit, direction="increasing", 
                                     mmdvec=c(10, 25, 50, 75, 100))

samplesizeGraph(canine.samplesize)

#### Paired Difference data
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)
anorexiaFT.fit <- fit(anorexiaFT.data)

## The interest is in increased weight for the anorexia FT
## (family treatment) group of patients
anorexiaFT.samplesize <- samplesizeTable(anorexiaFT.fit, direction="increasing",
                         mmdvec=c(5, 10, 15, 20)) ## print method shows table

samplesizeGraph(anorexiaFT.samplesize)

}