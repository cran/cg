\name{show.cgPairedDifferenceSampleSizeTable}
\docType{methods}
\alias{show,cgPairedDifferenceSampleSizeTable-method}
\alias{show.cgPairedDifferenceSampleSizeTable}
\title{
  Show a Paired Difference Sample Size Table object from the cg package
}
\description{
  Show a \code{cgPairedDifferenceSampleSizeTable} object, which contains a table of 
  sample size estimates based on a \code{cgPairedDifferenceFit} 
  object.
}
\usage{
\S4method{show}{cgPairedDifferenceSampleSizeTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgPairedDifferenceSampleSizeTable} object, 
    typically created by \cr\code{\link{samplesizeTable.cgPairedDifferenceFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceSampleSizeTable}} for details of the object
  slots. 
}
\value{
  \code{show.cgPairedDifferenceSampleSizeTable} returns
  \code{\link{invisible}}. The main purpose is the side
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
  \code{\link{cgPairedDifferenceSampleSizeTable}}, \code{\link{showDefault}}
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

## Recall the interest is in increased weight for the anorexia FT
## (family treatment) group of patients
show(samplesizeTable(anorexiaFT.fit, direction="increasing",
                     mmdvec=c(5, 10, 15, 20), display="none"))

}

