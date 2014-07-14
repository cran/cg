\name{showObj}
\docType{methods}
\alias{showObj}
\title{
  Show raw form of an object from the cg package
}
\description{
  Show the raw form of an object from the cg package.
}
\usage{
showObj(object)
}

\arguments{
  \item{object}{
    An object created by the \pkg{cg} package. 
  }
}
\details{
  The object raw form is shown using \code{\link{showDefault}}. The name
  \code{showObj} is designed for use when the conventional \code{show}
  name is an alias for \code{print} in the cg package. 
}
\value{
  A method-specific \code{fit} object is returned.
  See the specific methods for discussion of return values.
}
  
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
\code{\link{cgOneFactorFit}}, \code{\link{cgPairedDifferenceFit}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

showObj(canine.fit)

}

