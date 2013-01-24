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
samplesizeGraph(sstable, Nscale = "log", mmdscale = "log",
 cgtheme=TRUE, device = "single", \dots)
}
\arguments{
  \item{sstable }{
    A \code{samplesizeTable} created by a \code{samplesizeTable}
    method from the \pkg{cg} package. 
  }
  \item{Nscale }{
    A \code{character} indicating whether the y-axis, which shows the estimated
    samples sizes, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{mmdscale }{
    A \code{character} indicating whether the x-axis, which shows the minimum
    meaningful differences to be detected, should be drawn on the log scale (\code{"log"}) or
    the original scale (\code{"original"}).
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme. Namely \code{background},
    \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
  }
  \item{device }{
    Can be one of three values:

   \describe{
     \item{\code{"single"}}{The default, which will put all graph panels on the same
       device page.}
     \item{\code{"multiple"}}{Relevant only when more than one panel of
       graphs is possible. In
       that case, a new graphics device is generated each newly
       generated
       single-paneled graph.}
     \item{\code{"ask"}}{Relevant only when more than one panel of
       graphs is possible. In
       that case, each are portrayed as a single-paneled graph, with the
       \code{ask=TRUE} argument specified in \code{\link{par}} so that
       the user input confirmation is needed before the graphs are
       drawn.
     }
   }
 }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object.  See the specific methods for additional
    details.
  }
}
%%\details{
%%
%%}
\value{
  The main purpose is the side
  effect of graphing to the current device. See the specific methods for
  discussion of any return values.
}
%%\references{
%%  
%%}
\author{
  Bill Pikounis and John Oleynick
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



}