\name{canine}
\alias{canine}
\docType{data}
\title{Canine Data Set in the cg package
}
\description{
  A data frame used to illustrate the \pkg{cg} package. It has a One Factor / One-Way /
  Unpaired Samples layout.
}
\usage{data(canine)}
\format{
  A 5-by-5 data frame with 5 numeric observations from an experiment on the following 5
  groups of beagle dogs.
  \describe{
    \item{\code{AE }}{castration plus estradiol and androstanediol}
    \item{\code{E1 }}{castration plus low dose estradiol}
    \item{\code{E2 }}{castration plus high dose estradiol}
    \item{\code{CC }}{castration alone}
    \item{\code{NC }}{no castration (normal controls)}
  }
}
\details{The \code{canine} data set that comes with the \pkg{cg} package
  is in \code{groupcolumns} format for the\cr
  \code{\link{prepareCGOneFactorData}} call. Each column represents a group, and
  the observations in that group's column are the individual response or
  outcome values.

  The 5 groups are regarded as levels of one factor in the
  \code{\link{prepareCGOneFactorData}}, \code{\link{fit}}, and other methods in
  the \pkg{cg} package.

  An alternative format of this data set is contained in
  \code{\link{canine.listfmt}}. See that help file for details,
  including how it would be read and \code{\link{prepare}}d by \pkg{cg}.

  The purpose of this experiment was to evaluate the effect of a
  physiological dose of estradiol on prostate growth in dogs using
  ultrasound. See the reference below for details. Comparisons amongst
  all five groups are of interest. 
}
\seealso{
  \code{\link{canine.listfmt}}, \code{\link{prepareCGOneFactorData}}
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\references{
  Rhodes, L., Ding, V.D.H., Kemp, R.K., Khan, M.S., Nakhla, A.M., Pikounis, B., Rosner, W., Saunders, H.M. and Feeney, W.P. (2000).  "Estradiol causes a dose dependent stimulation of prostate growth in castrate beagle dogs." \emph{The Prostate}, Volume 44, 8-18.
}
\examples{
data(canine)
str(canine)
}
\keyword{datasets}

