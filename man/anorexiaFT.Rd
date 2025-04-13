\name{anorexiaFT}
\alias{anorexiaFT}
\docType{data}
\title{AnorexiaFT Data Set in the cg package
}
\description{
  A data frame used to illustrate the \pkg{cg} package. It has a Paired
  Samples / Paired Difference layout.
}
\usage{data(anorexiaFT)}
\format{
  A 17-by-2 data frame from an study of young female anorexia
  patients. It is a subset of the  \code{\link[MASS]{anorexia}} data frame
  included in the \pkg{MASS} package. Out of the three groups there,
  only the factor level \code{FT} group makes up the \code{anorexiaFT}
  data frame. The endpoint is Weight in pounds (lbs), \code{FT}
  represents "Family Therapy" treament, and the paired levels are
  \describe{
    \item{\code{PreWt }}{Patient weight before treatment}
    \item{\code{PostWt}}{Patient weight after treatment}
  }
  See \code{\link[MASS]{anorexia}} in the \pkg{MASS} package for additional description.
}
\details{The \code{anorexiaFT} data set that comes with the \pkg{cg} package
  is in \code{groupcolumns} format for the\cr
  \code{\link{prepareCGPairedDifferenceData}} call. Each column
  represents a group, and each row represents an individual patient, or
  experimental unit. Each observation in a row that spans the two group
  columns are paired individual response or outcome values.

  The purpose of the study was to evaluate effects of
  multiple treatments on weight as a marker for anorexia, and to
  compare their relative effectiveness.

  See \code{\link[MASS]{anorexia}} in the \pkg{MASS} package for
  references and more details.
}
\seealso{
  \code{\link[MASS]{anorexia}}, \code{\link{prepareCGPairedDifferenceData}}
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\examples{
data(anorexiaFT)
str(anorexiaFT)
}
\keyword{datasets}

