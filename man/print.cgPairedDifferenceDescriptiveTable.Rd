\name{print.cgPairedDifferenceDescriptiveTable}
\docType{methods}
\alias{print,cgPairedDifferenceDescriptiveTable-method}
\alias{print.cgPairedDifferenceDescriptiveTable}
\title{
  Print a Paired Difference Descriptive Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceDescriptiveTable} object, which contains a table of 
  quantiles and other summary statistics of the data from a 
  \code{cgPairedDifferenceData} object.
}
\usage{
\S4method{print}{cgPairedDifferenceDescriptiveTable}(x, digits = NULL, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    A \code{cgPairedDifferenceDescriptiveTable} object, typically created by\cr
    \code{\link{descriptiveTable.cgPairedDifferenceData}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the \code{cgPairedDifferenceDescriptiveTable} object.
  }
  \item{title }{
    The title printed out with the table.  If \code{NULL},
    it is set to be "Descriptive Table of"
    the \code{analysisname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceDescriptiveTable} object.
  }
  \item{endptname }{
    The endpoint name of the data summarized in the table. If NULL, it is set to 
    the \code{endptname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceDescriptiveTable} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgPairedDifferenceDescriptiveTable}}
  for details of the \code{contents} and other object slots.
}
\value{
  \code{print.cgPairedDifferenceDescriptiveTable} returns
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
  \code{\link{cgPairedDifferenceDescriptiveTable}}
}
\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)
## Next two calls are equivalent
descriptiveTable(anorexiaFT.data)

print(descriptiveTable(anorexiaFT.data, display="none"))

## A change in title
print(descriptiveTable(anorexiaFT.data, display="none"),
      title="Quantiles and Summary Statistics")

}


