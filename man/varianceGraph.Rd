\name{varianceGraph}
\docType{methods}
\alias{varianceGraph}
\title{
   Variance Graphs
}
\description{
  Create an equal variance assessment graph of the residuals of a fitted object
  from the \pkg{cg} package 
}
\usage{
varianceGraph(fit, trend = NULL, cgtheme = TRUE,
 device = "single", \ldots)  

}
\arguments{
  \item{fit }{
   A fit object, typically created by the \code{fit} generic function.
 }
 \item{trend }{
   Add a trend line to help assess the trend of the residuals. See
    specific method written for the \code{fit} argument.
  }
  \item{cgtheme }{
   When set to the default \code{TRUE}, ensures a trellis device is active with
   limited color scheme. Namely \code{background},
   \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
 }
 \item{device }{Can be one of three values:

   \describe{
     \item{\code{"single"}}{The default, which will put all graphs on the same
       device page.}
     \item{\code{"multiple"}}{Relevant only when multiple fitted models
     are present in the \code{fit} object and requested to be plotted.
       In
       those cases, a new graphics device is generated to hold each additional
       plot beyond the first.}
     \item{\code{"ask"}}{Relevant only when multiple fitted models
     are present in the \code{fit} object and requested to be plotted.
       In
       these cases, each plot is portrayed as a single-paneled graph, with the
       \code{ask=TRUE} argument specified in \code{\link{par}} so that
       the user input confirmation is needed before the graphs are
       drawn.
       }
     }

   }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the object.  See the method-specific documentation for additional
    details.
  }
}
\details{
The graphs plot the square root of the absolute value of the residuals against
the fitted value.
}
\value{
  \code{varianceGraph} returns
  an invisible \code{NULL}. The main purpose, of course, is the side
  effect of graphing to the current device.
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
  \code{\link{varianceGraph.cgOneFactorFit}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

varianceGraph(canine.fit)

varianceGraph(canine.fit, model="olsonly")
}

\concept{equal variance}
\concept{residual diagnostics}