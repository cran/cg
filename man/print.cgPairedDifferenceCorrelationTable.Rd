\name{print.cgPairedDifferenceCorrelationTable}
\docType{methods}
\alias{print,cgPairedDifferenceCorrelationTable-method}
\alias{print.cgPairedDifferenceCorrelationTable}
\title{
  Print a Paired Difference Correlation Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceCorrelationTable} object, which contains a table of 
  correlations of the data from a 
  \code{cgPairedDifferenceData} object.
}
\usage{
\S4method{print}{cgPairedDifferenceCorrelationTable}(x, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    A \code{cgPairedDifferenceCorrelationTable} object, typically created by\cr
    \code{\link{correlationTable.cgPairedDifferenceData}}.
  }
  \item{title }{
    The title printed out with the table.  If \code{NULL},
    it is set to be "Correlation Table of"
    the \code{analysisname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceCorrelationTable} object.
  }
  \item{endptname }{
    The endpoint name of the data summarized in the table. If NULL, it is set to 
    the \code{endptname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceCorrelationTable} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgPairedDifferenceCorrelationTable}}
  for details of the \code{contents} and other object slots.

  Two decimal places are used in the display of the correlations.
}
\value{
  \code{print.cgPairedDifferenceCorrelationTable} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
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
  \code{\link{cgPairedDifferenceCorrelationTable}}
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
correlationTable(anorexiaFT.data)

print(correlationTable(anorexiaFT.data, display="none"))

## A change in title
print(correlationTable(anorexiaFT.data, display="none"),
      title="Correlations")

}


