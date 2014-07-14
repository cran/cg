\name{showObj.cgOneFactorFit}
\docType{methods}
\alias{showObj.cgOneFactorFit}
\alias{showObj,cgOneFactorFit-method}
\title{
  Show a Fitted Model object from the cg package
}
\description{
  Show the raw form of a \code{cgOneFactorFit} object, which contains model fit
  information.
}
\usage{
\S4method{showObj}{cgOneFactorFit}(object)
}

\arguments{
  \item{object}{
    A \code{cgOneFactorFit} object.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorFit}}
  for details of the object slots.

  The name \code{showObj} is designed for use for cases like this when
  the coventional \code{show} name is an alias for \code{print}.
}
\value{
  \code{showObj.cgOneFactorFit} returns \code{\link{invisible}}.
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
\code{\link{cgOneFactorFit}}, \code{\link{showDefault}}
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

show(canine.fit) ## alias for print method on the object 
}

