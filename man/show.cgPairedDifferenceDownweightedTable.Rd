\name{show.cgPairedDifferenceDownweightedTable}
\docType{methods}
\alias{show.cgPairedDifferenceDownweightedTable}
\alias{show,cgPairedDifferenceDownweightedTable-method}
\title{
  Show a Paired Difference Downweighted Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceDownweightedTable} object, which contains
  information of downweighted observations in a resistant & robust fit
  from a \code{cgPairedDifferenceFit} object.
}
\usage{
\S4method{show}{cgPairedDifferenceDownweightedTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceDownweightedTable} object, typically created by 
    \cr\code{\link{downweightedTable.cgPairedDifferenceFit}}.
}
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceDownweightedTable}}
  for details of the object slots.
}
\value{
  \code{show.cgPairedDifferenceDownweightedTable} returns \code{\link{invisible}}.
  The main purpose is the side effect of printing the whole object to the current output
  connection, which is typically the console.
}  
\author{
  Bill Pikounis, John Oleynick, Eva Ye
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
\code{\link{cgPairedDifferenceDownweightedTable}}, \code{\link{showDefault}}
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
show(downweightedTable(anorexiaFT.fit, cutoffwt=0.25, display="none"))
}

