\name{show.cgOneFactorGlobalTest}
\docType{methods}
\alias{show.cgOneFactorGlobalTest}
\alias{show,cgOneFactorGlobalTest-method}
\title{
  Show a Global Test object from the cg package
}
\description{
  Show a \code{cgOneFactorGlobalTest} object, which contains p-value
  information from a \code{cgOneFactorFit} object.
}
\usage{
\S4method{show}{cgOneFactorGlobalTest}(object)
}

\arguments{
  \item{object}{
    A \code{cgOneFactorGlobalTest} object, typically created by 
    \cr\code{\link{globalTest.cgOneFactorFit}}.
}
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorGlobalTest}}
  for details of the object slots.
}
\value{
  \code{show.cgOneFactorGlobalTest} returns \code{\link{invisible}}.
  The main purpose is the side
  effect of printing the whole object to the current output connection, which is
  typically the console.
}
  
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
\code{\link{cgOneFactorGlobalTest}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
globalTest(canine.fit, display="show")
show(globalTest(canine.fit, display="none"))
}

