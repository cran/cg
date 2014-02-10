\name{show.cgPairedDifferenceCorrelationTable}
\docType{methods}
\alias{show.cgPairedDifferenceCorrelationTable}
\alias{show,cgPairedDifferenceCorrelationTable-method}
\title{
  Show a Paired Difference Correlation Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceCorrelationTable} object, which contains a table of 
  correlations of the data from a 
  \code{cgPairedDifferenceData} object.
}
\usage{
\S4method{show}{cgPairedDifferenceCorrelationTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceCorrelationTable} object, typically created by 
    \cr\code{\link{correlationTable.cgPairedDifferenceData}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceCorrelationTable}}
  for details of the object slots.
}
\value{
  \code{show.cgPairedDifferenceCorrelationTable} returns \code{\link{invisible}}.
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
\code{\link{cgPairedDifferenceCorrelationTable}}, \code{\link{showDefault}}
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
correlationTable(anorexiaFT.data, display="show")

show(correlationTable(anorexiaFT.data, display="none"))


}


