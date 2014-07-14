\name{show.cgPairedDifferenceComparisonsTable}
\docType{methods}
\alias{show.cgPairedDifferenceComparisonsTable}
\alias{show,cgPairedDifferenceComparisonsTable-method}
\title{
  Show a Paired Difference Data Comparisons Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceComparisonsTable} object, which contains
  information of comparisons based on a fit
  in a \code{cgPairedDifferenceFit} object.
}
\usage{
\S4method{show}{cgPairedDifferenceComparisonsTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceComparisonsTable} object, typically created by 
    \cr\code{\link{comparisonsTable.cgPairedDifferenceFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceComparisonsTable}}
  for details of the object slots.
}
\value{
  \code{show.cgPairedDifferenceComparisonsTable} returns \code{\link{invisible}}.
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
\code{\link{cgPairedDifferenceComparisonsTable}}, \code{\link{showDefault}}
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
show(comparisonsTable(anorexiaFT.fit, display="none"))
}

