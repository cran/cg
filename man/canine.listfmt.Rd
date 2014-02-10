\name{canine.listfmt}
\alias{canine.listfmt}
\docType{data}
\title{Canine Data Set in the cg package
}
\description{
  A data frame used to illustrate the \pkg{cg} package. It has a One Factor / One-Way /
  Unpaired Samples layout.
}
\usage{data(canine.listfmt)}
\format{
  A 25-by-2 data frame with 5 numeric observations from an experiment on
  each of the following 5 groups of beagle dogs.
  \describe{
    \item{\code{AE }}{castration plus estradiol and androstanediol}
    \item{\code{E1 }}{castration plus low dose estradiol}
    \item{\code{E2 }}{castration plus high dose estradiol}
    \item{\code{CC }}{castration alone}
    \item{\code{NC }}{no castration (normal controls)}
  }
  The above 5 items are the levels of the first column's factor, named
  \code{grp}. The second column \code{size} contains the numeric observations.
}
\details{The \code{canine.listfmt} data set that comes with the \pkg{cg} package
  is in \code{listed} format for the\cr
  \code{\link{prepareCGOneFactorData}} call. 

  The 5 groups are regarded as levels of one factor in the
  \code{\link{prepareCGOneFactorData}}, \code{\link{fit}}, and other methods in
  the \pkg{cg} package.

  \code{canine.listfmt} is an alternative format of the
  \code{\link{canine}} data set. See that help file for details. Once
  the data set is \code{\link{prepare}}d into a
  \code{\link{cgOneFactorData}} object, all the subsequent methods work
  on the object in the same way.

  The purpose of this experiment was to evaluate the effect of a
  physiological dose of estradiol on prostate growth in dogs using
  ultrasound. See the reference below for details. Comparisons amongst
  all five groups are of interest. 
}
\seealso{
  \code{\link{canine}}, \code{\link{prepareCGOneFactorData}}
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments.
}
\references{
  Rhodes, L., Ding, V.D.H., Kemp, R.K., Khan, M.S., Nakhla, A.M., Pikounis, B., Rosner, W., Saunders, H.M. and Feeney, W.P. (2000).  "Estradiol causes a dose dependent stimulation of prostate growth in castrate beagle dogs." \emph{The Prostate}, Volume 44, 8-18.
}
\examples{
data(canine.listfmt)
str(canine.listfmt)

## Analogous to prepareCGOneFactorData call on canine data frame format,
## subsequent methods will work for canine.listfmt.data:

canine.listfmt.data <- prepareCGOneFactorData(canine.listfmt, format="listed",
                                              analysisname="Canine",
                                              endptname="Prostate Volume",
                                              endptunits=expression(plain(cm)^3),
                                              digits=1, logscale=TRUE,
                                              refgrp="CC")
## as they do on canine.data:

canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
}
\keyword{datasets}

