\name{print.cgPairedDifferenceDownweightedTable}
\docType{methods}
\alias{print,cgPairedDifferenceDownweightedTable-method}
\alias{print.cgPairedDifferenceDownweightedTable}
\title{
  Print Downweighted Observations Table object with some format options
}
\description{
  Print a \code{cgPairedDifferenceDownweightedTable} object, as a
  table of downweighted observations in a resistant & robust fit from a
  \code{cgPairedDifferenceFit} object.
}
\usage{
\S4method{print}{cgPairedDifferenceDownweightedTable}(x, digits=NULL, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{x }{
    An object of class \code{\link{cgPairedDifferenceDownweightedTable}},
    typically created by \code{\link{downweightedTable.cgPairedDifferenceFit}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the 
    \code{\link{cgPairedDifferenceDownweightedTable}} object. 
  }
  \item{title }{
    The title printed out with the p-value.
    If \code{NULL}, it is set to be "Downweighted Observations Table from Resistant & Robust Fit"
    of the \code{analysisname} value in the settings of the 
    \code{\link{cgPairedDifferenceDownweightedTable}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the p-value. 
    If \code{NULL}, it is set to 
    the \code{endptname} value in the 
    \code{\link{cgPairedDifferenceDownweightedTable}} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
    }   
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgPairedDifferenceDownweightedTable}}
  for details of the \code{contents} and other object slots.
}
\value{\code{print.cgPairedDifferenceDownweightedTable} returns
  \code{\link{invisible}}. The main purpose is the side
  effect of printing to the current output connection, which is
  typically the console. If any observations meet the cutoff criteria, a
  table is displayed.

  If no observations meet the cutoff criteria, a text message of 
  table emptiness is displayed instead.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{cgPairedDifferenceDownweightedTable}}
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

anorexiaFT.dw <- downweightedTable(anorexiaFT.fit, cutoffwt=0.25, display='none') 

print(anorexiaFT.dw)  ## No observation

}


