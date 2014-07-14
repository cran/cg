\name{print.cgPairedDifferenceVarianceTable}
\docType{methods}
\alias{print,cgPairedDifferenceVarianceTable-method}
\alias{print.cgPairedDifferenceVarianceTable}
\title{
  Print a Paired Difference Variance Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceVarianceTable} object, which contains a table of 
  variances from a 
  \code{cgPairedDifferenceFit} object.
}
\usage{
\S4method{print}{cgPairedDifferenceVarianceTable}(x, digits = NULL, title = NULL, endptname = NULL, \dots)
}
\arguments{
  \item{x }{
    A \code{cgPairedDifferenceVarianceTable} object, created by\cr
    \code{\link{varianceTable.cgPairedDifferenceFit}}.
  }
  \item{digits }{ 
    The number of decimal places to use in the output, after any leading
    zeroes right of the decimal point.
    If \code{NULL}, then
    the number of decimal places is taken from the \code{digits} value
    in the \code{settings} slot of the\cr 
    \code{\link{cgPairedDifferenceComparisonsTable}} object. 
  }
  \item{title }{
    The title printed out with the table.  If \code{NULL},
    it is set to be "Variance Components Table of"
    the \code{analysisname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceVarianceTable} object.
  }
  \item{endptname }{
    The endpoint name of the data summarized in the table. If NULL, it is set to 
    the \code{endptname} value taken from the \code{settings} slot of the
    \cr\code{cgPairedDifferenceVarianceTable} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
  }
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See
  \code{\link{cgPairedDifferenceVarianceTable}}
  for details of the \code{contents} and other object slots.

  Two decimal places (after any leading zeroes) are used by default in
  the display of the variances.

  As described in \code{\link{cgPairedDifferenceVarianceTable}}, the
  table displays a decomposition of the total variance into its
  within-experimental unit  and between-experimential
  unit variance compoments. The variance estimates are provided in the
  first column, and the relative percents of these two components are in
  the second column. The third column is the square root of the
  first column of variances, to provide \code{Spread/StdDev} values in
  the units of the endpoint.

  Below the printed table is a series of Notes. The first note narrates
  the estimated gain in sensitivity from using a paired groups design
  instead of an unpaired groups design. The gains are expressed in terms of reduced
  experimental unit sample size.

  The label portion "experimental unit" in the printed output
  is replaced by the \code{expunitname} component of the
  \code{settings} slot of the \code{\link{cgPairedDifferenceVarianceTable}}
  object.
}
\value{
  \code{print.cgPairedDifferenceVarianceTable} returns
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
  \code{\link{cgPairedDifferenceVarianceTable}}
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
varianceTable(anorexiaFT.fit)

print(varianceTable(anorexiaFT.fit, display="none"))

## A change in title
print(varianceTable(anorexiaFT.fit, display="none"),
      title="Estimated Variances")

## Show three digits in display 
print(varianceTable(anorexiaFT.fit), digits=3)

}


