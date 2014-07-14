\name{show.cgPairedDifferenceDescriptiveTable}
\docType{methods}
\alias{show.cgPairedDifferenceDescriptiveTable}
\alias{show,cgPairedDifferenceDescriptiveTable-method}
\title{
  Show a Paired Difference Descriptive Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceDescriptiveTable} object, which contains a table of 
  quantiles and other summary statistics of the data from a 
  \code{cgPairedDifferenceData} object.
}
\usage{
\S4method{show}{cgPairedDifferenceDescriptiveTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceDescriptiveTable} object, typically created by 
    \cr\code{\link{descriptiveTable.cgPairedDifferenceData}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceDescriptiveTable}}
  for details of the object slots.
}
\value{
  \code{show.cgPairedDifferenceDescriptiveTable} returns \code{\link{invisible}}.
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
\code{\link{cgPairedDifferenceDescriptiveTable}}, \code{\link{showDefault}}
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
descriptiveTable(anorexiaFT.data, display="show")

show(descriptiveTable(anorexiaFT.data, display="none"))


}


