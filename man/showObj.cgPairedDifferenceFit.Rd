\name{showObj.cgPairedDifferenceFit}
\docType{methods}
\alias{showObj.cgPairedDifferenceFit}
\alias{showObj,cgPairedDifferenceFit-method}
\title{
  Show a Fitted Model object from the cg package
}
\description{
  Show the raw form of a \code{cgPairedDifferenceFit} object, which contains model fit
  information.
}
\usage{
\S4method{showObj}{cgPairedDifferenceFit}(object)
}

\arguments{
  \item{object}{
    A \code{cgPairedDifferenceFit} object.
  }
}
\details{
  The object is shown using \code{\link{showDefault}}. See
  \code{\link{cgPairedDifferenceFit}}
  for details of the object slots.

  The name \code{showObj} is designed for use for cases like this when
  the coventional \code{show} name is an alias for \code{print}.
}
\value{
  \code{showObj.cgPairedDifferenceFit} returns \code{\link{invisible}}.
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
  \code{\link{cgOneFactorFit}}, \code{\link{cgPairedDifferenceFit}},
  \code{\link{showDefault}}
}
\examples{
data(anorexiaFT)
anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1, logscale=TRUE)
anorexiaFT.fit <- fit(data=anorexiaFT.data, type="rr")	

showObj(anorexiaFT.fit)

show(anorexiaFT.fit) ## alias for print method on the object 
}

