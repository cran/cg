\name{show.cgOneFactorDescriptiveTable}
\docType{methods}
\alias{show.cgOneFactorDescriptiveTable}
\alias{show,cgOneFactorDescriptiveTable-method}
\title{
  Show an One Factor Descriptive Table object from the cg package
}
\description{
  Show a \code{cgOneFactorDescriptiveTable} object, which contains a table of 
  quantiles and other summary statistics of the data from a 
  \code{cgOneFactorData} object.
}
\usage{
\S4method{show}{cgOneFactorDescriptiveTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgOneFactorDescriptiveTable} object, typically created by 
    \cr\code{\link{descriptiveTable.cgOneFactorData}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorDescriptiveTable}}
  for details of the object slots.
}
\value{
  \code{show.cgOneFactorDescriptiveTable} returns \code{\link{invisible}}.
  The main purpose is the side
  effect of printing the whole object to the current output connection, which is
  typically the console.
}
\author{
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
\code{\link{cgOneFactorDescriptiveTable}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
show(descriptiveTable(canine.data, display="none"))

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
show(descriptiveTable(gmcsfcens.data, display="none"))
}


