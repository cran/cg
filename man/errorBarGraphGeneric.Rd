\name{errorBarGraph}
\docType{methods}
\alias{errorBarGraph}
\title{
   Create an Error Bar graph amongst groups
}
\description{
  Generic function to create a Error Bar graph based on a fit by the \pkg{cg}
  package.  
}
\usage{
errorBarGraph(fit, mcadjust=FALSE, alpha = 0.05,
 cgtheme = TRUE, device="single", ...) 
}
\arguments{
  \item{fit }{
    A fit object created by a \code{\link{fit}} method from the \pkg{cg}
    package.   
  }
  \item{mcadjust }{
    Do a multiple comparisons adjustment, based on the simultaneous
    inference capabilities of the \pkg{multcomp} package. See Details
    below. The default value is \code{FALSE}. 
    If \code{mcadjust=TRUE} is specified, there will be a delay,
    usually just for a few seconds, due to computing time of the
    critical point in order to create the error bar intervals.
  }
  \item{alpha }{Significance level, by default set to \code{0.05} so
    that confidence levels are 95\%.
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
    the object. See the method-specific documentation for additional
    details.
  }
}
\details{
  When \code{mcadjust=TRUE}, a status message of
  \code{"Some time may be needed"} \cr
  \code{"as the critical point from the multcomp::summary.glht function call is calculated"}\cr
    is displayed at the console. This computed critical point
  is used for all subsequent p-value and confidence interval calculations.
}
\value{
  The main purpose is the side
  effect of graphing to the current device. See specific methods for
  discussion of any return values.
}
\references{
  Hothorn, T., Bretz, F., Westfall, P., Heiberger, R.M., and
  Schuetzenmeister, A. (2010). The \code{multcomp} R package.

  Hothorn, T., Bretz, F., and Westfall, P. (2008).
  "Simultaneous Inference in General Parametric Models",
  \emph{Biometrical Journal}, 50, 3, 346-363.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{errorBarGraph.cgOneFactorFit}}
}

\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

errorBarGraph(canine.fit)

errorBarGraph(canine.fit, mcadjust=TRUE, model="olsonly")
}
\concept{error bars}
\concept{comparison}

