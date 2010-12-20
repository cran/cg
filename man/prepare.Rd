\name{prepare}

\alias{prepare}
\title{
  Prepare a cg data object from a data frame
}
\description{
  Reads in a data frame and
  settings in order to create a
  \code{cg} Data object. 
}
\usage{
prepare(type, ...)
}
\arguments{
  \item{type }{Currently only accepts the value of \code{"onefactor"} or its
    synonym \code{"unpairedgroups"} to create a
    \code{\link{cgOneFactorData}} object.}
  \item{... }{Depends on the specific function that is called according
    to the \code{type} argument. Currently the
    \code{\link{prepareCGOneFactorData}} is the sole valid call, and no
    \dots arguments are used.
  }
}
\value{
  See \code{\link{cgOneFactorData}} for the only possible valid object
  currently returned, assuming that the \code{type} and \code{\dots}
  arguments are correctly specified.
}
\author{
  Bill Pikounis and John Oleynick 
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{prepareCGOneFactorData}}
}
\examples{
data(canine)
canine.data <- prepare(type="unpairedgroups", dfr=canine,
                       format="groupcolumns",
                       analysisname="Canine",
                       endptname="Prostate Volume",
                       endptunits=expression(plain(cm)^3),
                       digits=1, logscale=TRUE, refgrp="CC")

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepare(type="onefactor",
                          dfr=gmcsfcens, format="groupcolumns",
                          analysisname="cytokine",
                          endptname="GM-CSF (pg/ml)",
                          logscale=TRUE)

}
\keyword{datagen}
