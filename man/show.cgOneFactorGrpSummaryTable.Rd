\name{show.cgOneFactorGrpSummaryTable}
\docType{methods}
\alias{show.cgOneFactorGrpSummaryTable}
\alias{show,cgOneFactorGrpSummaryTable-method}
\title{
  Show a One Factor Group Summary Table object from the cg package
}
\description{
  Show a \code{cgOneFactorGrpSummaryTable} object, which contains
  information of group mean and standard error summaries based on a fit
  in a \code{cgOneFactorFit} object.
}
\usage{
\S4method{show}{cgOneFactorGrpSummaryTable}(object)
}
\arguments{
  \item{object}{
    A \code{cgOneFactorGrpSummaryTable} object, typically created by 
    \cr\code{\link{grpSummaryTable.cgOneFactorFit}}.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgOneFactorGrpSummaryTable}}
  for details of the object slots.
}
\value{
  \code{show.cgOneFactorGrpSummaryTable} returns \code{\link{invisible}}.
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
\code{\link{cgOneFactorGrpSummaryTable}}, \code{\link{showDefault}}
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)
show(grpSummaryTable(canine.fit, display="none"))
}

