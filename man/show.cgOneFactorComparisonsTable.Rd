\name{show.cgOneFactorComparisonsTable}
\docType{methods}
\alias{show.cgOneFactorComparisonsTable}
\alias{show,cgOneFactorComparisonsTable-method}
\title{
  Show a One Factor Comparisons Table object from the cg package
}
\description{
  Show a \code{cgOneFactorComparisonsTable} object, which contains
  information of comparisons based on a fit
  in a \code{cgOneFactorFit} object.
}
\usage{
\S4method{show}{cgOneFactorComparisonsTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgOneFactorComparisonsTable} object, typically created by 
    \cr\code{\link{comparisonsTable.cgOneFactorFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorComparisonsTable}}
  for details of the object slots.
}
\value{
  \code{show.cgOneFactorComparisonsTable} returns \code{\link{invisible}}.
  The main purpose is the side
  effect of printing the whole object to the current output connection, which is
  typically the console.
}  
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
\code{\link{cgOneFactorComparisonsTable}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
show(comparisonsTable(canine.fit, display="none"))
}

