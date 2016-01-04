\name{cg-package}
\alias{cg-package}
\alias{cg}
\docType{package}
\title{ Compare Groups, Analytically and Graphically }
\description{ cg is comprehensive data analysis software, and stands for
"compare groups." Its genesis and evolution are driven by common needs
to compare administrations, conditions, etc. in medicine research and
development. The current version provides comparisons of unpaired
samples, i.e. a linear model with one factor of at least two levels. It
also provides comparisons of two paired samples. Good data graphs,
modern statistical methods, and useful displays of results are emphasized.
}
\details{
Package: cg\cr
Type: Package\cr
Version: 1.0-3\cr
Date: 2016-01-05\cr
License: GPL (>= 2)\cr
LazyLoad: yes\cr
LazyData: yes\cr
Depends: R (>= 3.2.3), Hmisc (>= 3.17-1)
Imports: VGAM (>= 1.0-0), methods, grDevices, graphics, stats, utils, grid, MASS, lattice, survival, multcomp, nlme, rms
}
\author{
Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]

Maintainer: Bill Pikounis <cg@billpikounis.net>

}
\references{
  Pikounis, B. and Oleynick, J. (2013).  "The cg Package for Comparison of Groups",
  \emph{Journal of Statistical Software}, Volume 52, Issue 1, 1-27, \url{http://www.jstatsoft.org/v52/i01/}.
}
\examples{
#### One Factor data

data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
## Exploratory methods
pointGraph(canine.data)

boxplot(canine.data)

descriptiveTable(canine.data)

## Fits and Comparisons
canine.fit <- fit(canine.data)

canine.comps0 <- comparisonsTable(canine.fit)

errorBarGraph(canine.fit)

canine.comps1 <- comparisonsTable(canine.fit,  mcadjust=TRUE,
                                   type="allgroupstocontrol", refgrp="CC")

comparisonsGraph(canine.comps1)

grpSummaryTable(canine.fit)

## Diagnostics
varianceGraph(canine.fit)

qqGraph(canine.fit)

downweightedTable(canine.fit, cutoff=0.95)

## Sample Size calculations
canine.samplesize <- samplesizeTable(canine.fit, direction="increasing", 
                                     mmdvec=c(10, 25, 50, 75, 100))

samplesizeGraph(canine.samplesize)

## Censored Data Set
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
pointGraph(gmcsfcens.data)

boxplot(gmcsfcens.data)

descriptiveTable(gmcsfcens.data)

gmcsfcens.fit <- fit(gmcsfcens.data, type="aft")

## Paired Samples

data(anorexiaFT)

anorexiaFT.data <- prepareCGPairedDifferenceData(anorexiaFT, format="groupcolumns",
                                                 analysisname="Anorexia FT",
                                                 endptname="Weight",
                                                 endptunits="lbs",
                                                 expunitname="Patient",
                                                 digits=1,
                                                 logscale=TRUE)

## Exploratory methods
descriptiveTable(anorexiaFT.data)

profileGraph(anorexiaFT.data)

diffGraph(anorexiaFT.data)

## Fits and Comparisons
anorexiaFT.fit <- fit(anorexiaFT.data)

comparisonsTable(anorexiaFT.fit)


}
\keyword{ package }
\concept{compare groups}
\concept{comparisons}
\concept{point graph}
\concept{boxplot}
\concept{resistant}
\concept{robust}
\concept{censored}
\concept{diagnostics}
\concept{samplesize}
