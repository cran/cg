\name{comparisonsGraph}
\docType{methods}
\alias{comparisonsGraph}
\title{
   Graph comparisons specified amongst groups
}
\description{
  Generic function to create a Comparisons Graph based on a Comparisons
  Table created in turn by the \pkg{cg}
  package.  
}
\usage{
comparisonsGraph(compstable, cgtheme=TRUE, device="single",
                 wraplength=20, cex.comps=0.7, \dots) 
}
\arguments{
  \item{compstable }{
    A \code{comparisonsTable} object created by a
    \code{\link{comparisonsTable}} method from the \pkg{cg} package. 
    \cr There is one class of objects that is currently available:
    \cr\code{\link{cgOneFactorComparisonsTable}}, which is prepared by the 
    \cr\code{\link{comparisonsTable.cgOneFactorFit}} method.
  }
  \item{cgtheme }{
    When set to the default \code{TRUE}, ensures a trellis device is active with
    limited color scheme. Namely, \code{background},
    \code{strip.shingle} and \code{strip.background} are each set to \code{"white"}.
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
       user input confirmation is needed before the graphs are
       drawn.
     }
   }
 }
 \item{wraplength }{On the left hand vertical axis are each A vs. B comparison label
   from the \code{compstable} object. An attempt at sensible formatting
   when a newline is needed is made, but adjustment by this argument may
   be needed. The default is \code{20} characters before wrapping to a newline.
 }
 \item{cex.comps }{Similar to \code{wraplength},
   adjustment of this argument parameter can
   be made to fit the comparison labels on the left hand vertical axis.
 }
 \item{\dots }{
    Additional arguments, depending on the specific method written for
    the \code{compstable} object. Currently, there is only one such specific method; see
    \cr \code{\link{comparisonsGraph.cgOneFactorComparisonsTable}} for any additional
    arguments that can be specified.
  }
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
  \code{\link{comparisonsGraph.cgOneFactorComparisonsTable}}
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

canine.comps1 <- comparisonsTable(canine.fit,  mcadjust=TRUE,
                                  type="allgroupstocontrol", refgrp="CC")

comparisonsGraph(canine.comps1)



}
