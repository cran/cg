\name{show.cgPairedDifferenceVarianceTable}
\docType{methods}
\alias{show.cgPairedDifferenceVarianceTable}
\alias{show,cgPairedDifferenceVarianceTable-method}
\title{
  Show a Paired Difference Variance Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceVarianceTable} object, which contains a table of 
  variances  from a 
  \code{cgPairedDifferenceFit} object.
}
\usage{
\S4method{show}{cgPairedDifferenceVarianceTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceVarianceTable} object, typically created by 
    \cr\code{\link{varianceTable.cgPairedDifferenceFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceVarianceTable}}
  for details of the object slots.
}
\value{
  \code{show.cgPairedDifferenceVarianceTable} returns \code{\link{invisible}}.
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
\code{\link{cgPairedDifferenceVarianceTable}}, \code{\link{showDefault}}
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
anorexiaFT.fit <- fit(anorexiaFT.data)

## Next two calls are equivalent
varianceTable(anorexiaFT.fit, display="show")

show(varianceTable(anorexiaFT.fit, display="none"))


}


