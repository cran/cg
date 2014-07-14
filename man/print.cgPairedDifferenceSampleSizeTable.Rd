\name{print.cgPairedDifferenceSampleSizeTable}
\docType{methods}
\alias{print,cgPairedDifferenceSampleSizeTable-method}
\alias{print.cgPairedDifferenceSampleSizeTable}
\title{
  Print a Paired Difference Sample Size Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceSampleSizeTable} object, which contains a table of 
  sample size estimates based on a \code{cgPairedDifferenceFit} 
  object.
}
\usage{
\S4method{print}{cgPairedDifferenceSampleSizeTable}(x, title=NULL, endptname=NULL, \dots)
}
\arguments{
  \item{x}{
    A \code{cgPairedDifferenceSampleSizeTable} object,
    typically created by\cr 
    \code{\link{samplesizeTable.cgPairedDifferenceFit}}.
  }
  \item{title}{
    The title for the table.  If \code{NULL}, it is set to be "Sample Size Table from"
    concatenated to \code{planningname} value
    in the \code{settings} slot of the\cr
    \code{cgPairedDifferenceSampleSizeTable} object.
  }
  \item{endptname}{
    The endpoint name, printed out with the table. 
    If \code{NULL}, it is set to 
    the \code{endptname} value from the \code{settings} slot of the  
    \code{cgPairedDifferenceSampleSizeTable} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined. 
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgPairedDifferenceSampleSizeTable}}
  for details of the \code{*.sstable} and other object slots.
}
\value{
  \code{print.cgPairedDifferenceSampleSizeTable} returns \code{\link{invisible}}.
  The main purpose is the side
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
  \code{\link{cgPairedDifferenceSampleSizeTable}}
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

## The interest is in increased weight for the anorexia FT
## (family treatment) group of patients

anorexiaFT.samplesize <- samplesizeTable(anorexiaFT.fit, direction="increasing",
                         mmdvec=c(5, 10, 15, 20), display="none")

print(anorexiaFT.samplesize)

## The above two calls produce the same screen output as
samplesizeTable(anorexiaFT.fit, direction="increasing",
                mmdvec=c(5, 10, 15, 20))

## since the default in the call is display="print"

}
