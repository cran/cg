\name{errorbargraph}

\alias{errorbargraph}
\title{
   Create an error bar graph based on pairwise multiple comparisons 
}
\description{
  Creates a graph to see pairwise comparisons amongst groups.
  The method of Andrews, Sarner, and Snee (1980) is applied to
  visualizes significant differences via non-overlapping error bars.
}
\usage{
errorbargraph(estimates, centralvar, critpoint, endptscale="log",
 analysisname="", endptname="", alpha=0.05, digits=NULL, approxstamp=FALSE,
 titlestamp=TRUE, offset=NULL, ticklabels=NULL, ...)
}
\arguments{
  \item{estimates }{
   A named vector of estimates. Each estimate element is a measure that will be
   the center of the error bar of the group. The name of each group must
   be present in the names attribute of the vector.
 }
 \item{centralvar }{
   A single variance value to be used for each group's error bar
   construction. In the canonical case it is the \emph{square of} the estimated
   standard error of the mean \code{estimate} of the group,
   where each group also has the same standard error (and sample size).
   If the standard errors / variances are similar enough across the groups, the
   visualization may still be effective even though the error bar
   lengths will be approximations.
 }
  \item{critpoint }{
   The single critical value of the theoretical reference distribution.  In the
   canonical case it is the t-distribution quantile for estimates derived from  a
   standard linear model with homoscedastic variance. It could also
   reflect a multiplicity adjustment, or like the
   \code{centralvar} discussion in the previous item, it may serve as
   part of a visually useful approximation for other cases.
 }
 \item{endptscale }{Must be specified as \code{"log"} or
   \code{"original"}. If the default \code{"log"} then the y-axis will be created
   with a logarithmic spacing. The tick marks will be calculated accordingly and
   expressed in the original scale of the estimates. In other words, the
   \code{estimates} vector must not be already transformed.
 }
 \item{analysisname }{\emph{Optional}, a character text or
    math-valid expression that will be set for
   default use in graph title and table methods. The default
   value is the empty \code{""}.}
 \item{endptname }{\emph{Optional}, a character text or math-valid expression
   that will be set for default use as the y-axis label of graph
   methods, and also used for table methods. The default
    value is the empty \code{""}.}
  \item{alpha }{Significance level, by default set to \code{0.05}, which
    equates to a 95\% confidence level. This
    is just used for labelling purposes.
  }
  \item{digits }{\emph{Optional}, for output display purposes in graphs
    and table methods, values will be rounded to this numeric
    value. Only the integers of 0, 1, 2, 3, and 4 are accepted. No
    rounding is done during any calculations. The default value is
    \code{NULL}, which will examine each individual \code{estimates}
    value and choose the
    one that has the maximum number of digits after any trailing
    zeroes are ignored. The max number of digits will be 4.}
  \item{approxstamp }{Add text to the graph that acknowledges that the
    error bar method is approximate.}
  \item{titlestamp }{Add text to the top margin above the graph area.}
  \item{offset }{\emph{Optional}, 
    if for example a numeric constant was added to all response values
    before calculation of the estimate as a mean, this could be used to
    shift the axis marks appropriately. The default value is
    \code{NULL}.}
  \item{ticklabels }{\emph{Optional}, 
    before graphing the data, remove
    any automatically generated tickmarks for the y-axis,
    and use these tickmarks instead. A vector of tickmarks to be
    placed on the y-axis. Any numeric representations will
    be coerced to character.
  }
  \item{\dots }{
    Additional arguments. None are currently used.
  }
}
\section{Warning}{
  This function was created for internal use in the \pkg{cg} package as
  its use can be seen in the \code{\link{errorBarGraph}} methods
  code. Therefore any direct use of it needs to be done cautiously.
}
\details{
  The statistical method of Andrews, Sarner, and Snee (1980) is applied to
  visualizes significant differences via non-overlapping error bars. The
  method is exact when there are equal standard errors amongst the
  groups, and approximate otherwise. The method's usefulness
  declines as the  standard errors become more disparate.

  When two groups are compared, nonoverlapping error bars indicate a
  statistically significant pairwise difference. Conversely, if the
  error bars overlap, there is no such significant difference. In cases
  of approximation, or borderline overlap that is seen, the
  actual comparison needs to be consulted to judge significance with a p-value.

  The minimum and maximum values across all the bar ends
  are added inside the plot region in blue, flush against the
  y-axis. The number of decimal places are determined by the
  \code{digits} value.
}
\value{
  \code{errorbargraph} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\references{
  Andrews, H.P., Snee, R.D., Sarner, M.H. (1980). "Graphical Display of Means,"
  \emph{The American Statistician}, 34, 195-199.
}
\author{
  Bill Pikounis and John Oleynick
}
\note{
  Contact \email{cg@billpikounis.net} for bug reports, questions,
  concerns, and comments. 
}
\examples{
data(canine)
canine.data <- prepareCGOneFactorData(canine, format="groupcolumns",
                                      analysisname="Canine",
                                      endptname="Prostate Volume",
                                      endptunits=expression(plain(cm)^3),
                                      digits=1, logscale=TRUE, refgrp="CC")
canine.fit <- fit(canine.data)

## Easier way: notice the camel case of the errorBarGraph call
errorBarGraph(canine.fit, model="olsonly")

## Manual way
## Instead of errorBarGraph(canine.fit, model="olsonly")
errorbargraph(estimates=canine.fit@olsfit$coef,
              centralvar=((summary(canine.fit@olsfit)$sigma^2) /
                          unique(sapply(canine, length))),
              critpoint=qt(0.975, df=canine.fit@olsfit$df.residual),
              endptscale="log",
              analysisname="Canine",
              digits=1,
              endptname=expression(paste( plain('Prostate Volume'),
                                      ' (', plain(cm)^3  ,  ')' ))
              )
              
}
\concept{error bars}
\concept{pairwise comparisons}
