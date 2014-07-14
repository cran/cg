\name{print.cgOneFactorDownweightedTable}
\docType{methods}
\alias{print,cgOneFactorDownweightedTable-method}
\alias{print.cgOneFactorDownweightedTable}
\title{
  Print Downweighted Observations Table object with some format options
}
\description{
  Print a \code{cgOneFactorDownweightedTable} object, as a
  table of downweighted observations in a resistant & robust fit from a
  \code{cgOneFactorFit} object.
}
\usage{
\S4method{print}{cgOneFactorDownweightedTable}(x, digits=NULL, title = NULL, endptname = NULL, ...)
}
\arguments{
  \item{x }{
    An object of class \code{\link{cgOneFactorDownweightedTable}},
    typically created by\cr \code{\link{downweightedTable.cgOneFactorFit}}.
  }
  \item{digits }{
    The number of decimal places to use in the output.  If \code{NULL}, then
    the number of decimal places is taken from the 
    \code{\link{cgOneFactorDownweightedTable}} object. 
  }
  \item{title }{
    The title printed out with the p-value.
    If \code{NULL}, it is set to be "Downweighted Observations Table from Resistant & Robust Fit"
    of the \code{analysisname} value in the settings of the 
    \code{\link{cgOneFactorDownweightedTable}} object.
  }
  \item{endptname }{
    The endpoint name, printed out with the p-value. 
    If \code{NULL}, it is set to 
    the \code{endptname} value in the 
    \code{\link{cgOneFactorDownweightedTable}} object.
  }
  \item{\dots }{
    Additional arguments. None are currently defined for this method.
    }   
}
\details{
  The object is printed using a mix of \code{cat} and \code{print}
  calls. See \code{\link{cgOneFactorDownweightedTable}}
  for details of the \code{contents} and other object slots.
}
\value{\code{print.cgOneFactorDownweightedTable} returns
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
  \code{\link{cgOneFactorDownweightedTable}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

canine.dwtable <- downweightedTable(canine.fit, cutoff=0.95)

downweightedTable(canine.fit, cutoff=0.75) ## No observation
                                           ## downweighted at least 25\%
}


