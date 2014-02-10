\name{print.cgOneFactorDescriptiveTable}
\docType{methods}
\alias{print,cgOneFactorDescriptiveTable-method}
\alias{print.cgOneFactorDescriptiveTable}
\title{
  Print a One Factor Descriptive Table object with some format options
}
\description{
  Print a \code{cgOneFactorDescriptiveTable} object, which contains a table of 
  quantiles and other summary statistics of the data from a 
  \code{cgOneFactorData} object.
}
\usage{
\S4method{print}{cgOneFactorDescriptiveTable}(x, digits = NULL, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    A \code{cgOneFactorDescriptiveTable} object, typically created by\cr
    \code{\link{descriptiveTable.cgOneFactorData}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the \code{cgOneFactorDescriptiveTable} object.
  }
  \item{title }{
    The title printed out with the table.  If \code{NULL},
    it is set to be "Descriptive Table of"
    the \code{analysisname} value taken from the \code{settings} slot of the
    \cr\code{cgOneFactorDescriptiveTable} object.
  }
  \item{endptname }{
    The endpoint name of the data summarized in the table. If NULL, it is set to 
    the \code{endptname} value taken from the \code{settings} slot of the
    \cr\code{cgOneFactorDescriptiveTable} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgOneFactorDescriptiveTable}}
  for details of the \code{contents} and other object slots.
}
\value{
  \code{print.cgOneFactorDescriptiveTable} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
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
  \code{\link{cgOneFactorDescriptiveTable}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")

## Next two calls are equivalent
descriptiveTable(canine.data)

print(descriptiveTable(canine.data, display="none"))

print(descriptiveTable(canine.data, display="none"),
      title="Quantiles and Summary Statistics")

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)

## Next two calls are equivalent
descriptiveTable(gmcsfcens.data, display="print")
print(descriptiveTable(gmcsfcens.data, display="none"))


}


