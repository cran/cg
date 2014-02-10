\name{show.cgOneFactorSampleSizeTable}
\docType{methods}
\alias{show,cgOneFactorSampleSizeTable-method}
\alias{show.cgOneFactorSampleSizeTable}
\title{
  Show a One Factor Sample Size Table object from the cg package
}
\description{
  Show a \code{cgOneFactorSampleSizeTable} object, which contains a table of 
  sample size estimates based on a \code{cgOneFactorFit} 
  object.
}
\usage{
\S4method{show}{cgOneFactorSampleSizeTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgOneFactorSampleSizeTable} object, 
    typically created by \cr\code{\link{samplesizeTable.cgOneFactorFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorSampleSizeTable}} for details of the object
  slots. 
}
\value{
  \code{show.cgOneFactorSampleSizeTable} returns
  \code{\link{invisible}}. The main purpose is the side
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
  \code{\link{cgOneFactorSampleSizeTable}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
show(samplesizeTable(canine.fit, direction="increasing", 
                     mmdvec=c(25, 50, 75, 100), display="none"))
}



               