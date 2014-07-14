\name{show.cgOneFactorDownweightedTable}
\docType{methods}
\alias{show.cgOneFactorDownweightedTable}
\alias{show,cgOneFactorDownweightedTable-method}
\title{
  Show a One Factor Downweighted Table object from the cg package
}
\description{
  Show a \code{cgOneFactorDownweightedTable} object, which contains
  information of downweighted observations in a resistant & robust fit
  from a \code{cgOneFactorFit} object.
}
\usage{
\S4method{show}{cgOneFactorDownweightedTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgOneFactorDownweightedTable} object, typically created by 
    \cr\code{\link{downweightedTable.cgOneFactorFit}}.
}
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorDownweightedTable}}
  for details of the object slots.
}
\value{
  \code{show.cgOneFactorDownweightedTable} returns \code{\link{invisible}}.
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
\code{\link{cgOneFactorDownweightedTable}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
show(downweightedTable(canine.fit, cutoffwt=0.95, display="none"))
}

