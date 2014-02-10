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
  \item{type }{Values and synonyms to create a
     \pkg{cg} data object. For one factor / unpaired samples, either
     "onefactor" or "unpairedgroups" can be specified. For paired
     samples, either "paireddifference" or "pairedgroups" can be
     used. Partial matching also allows shortened forms such as
     "unpaired" or "paireddiff".
  }
  \item{... }{Depends on the specific function that is called according
    to the \code{type} argument. For current valid calls, no
    \dots arguments are used.
  }
}
\value{
  See \code{\link{cgOneFactorData}} and
  \code{\link{cgPairedDifferenceData}}
  for possible valid objects that are created,
  dependent on the \code{type} and \code{\dots}
  arguments that are correctly specified.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye 
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\seealso{
  \code{\link{prepareCGOneFactorData}}, \code{\link{prepareCGPairedDifferenceData}}
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

## Paired Groups
data(anorexiaFT)
anorexiaFT.data <- prepare(type="paireddiff",  ## Partial matching
                           dfr=anorexiaFT, format="groupcolumns",
                           analysisname="Anorexia FT",
                           endptname="Weight",
                           endptunits="lbs",
                           expunitname="Patient",
                           digits=1, logscale=TRUE)
}
\keyword{datagen}



