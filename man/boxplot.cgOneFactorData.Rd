\name{boxplot.cgOneFactorData}
\docType{methods}
\alias{boxplot,cgOneFactorData-method}
\alias{boxplot.cgOneFactorData}
\title{Box Plot Graph of Groups from a cgOneFactorData object}
\description{
  Create graph of boxplots of groups in a \code{cgOneFactorData} object.
}
\usage{
\S4method{boxplot}{cgOneFactorData}(x, \dots)
}
\arguments{
  \item{x}{
    A \code{\link{cgOneFactorData}} object.
  }
  \item{\dots }{
    Additional arguments, both \emph{optional}. Two are currently valid:
    \describe{
      \item{\code{logscale}}{
	A \code{logical} value, indicating whether or not the boxplot graph should be
	plotted on the logarithmic scale.
	If \code{logscale} is not
	specified, its value is taken from the \code{cgOneFactorData} object,
	which \code{\link{prepareCGOneFactorData}} sets from its \code{logscale} argument.
      }
      \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the y-axis,
		and create the tickmarks specified in the
		\code{marks} component (see below).
	      }
	      \item{\code{"add"}}{Before graphing the data, add
		tickmarks specified in the
		\code{marks} component to the automatically
		generated ones.
	      }
	    }
	  }
	  \item{\code{marks}}{A vector of tickmarks to be
	    placed on the y-axis. Any numeric representations will
	    be coerced to chracter.
	  }
	}
      }
    }
  }
}
\details{
  For uncensored data, the boxplot for each group produced is a standard boxplot,
  similar to that produced by
  \pkg{graphics}::\code{\link{boxplot.default}},
  but with the median shown as a "+" and the mean shown as a "o".  A
  warning is added to the plot if any of the groups or all of the groups
  have 5 or fewer observations (in which case a plot from
  \code{\link{pointGraph.cgOneFactorData}} might be more suitable).

  For censored data, Kaplan-Meier estimates are used for the quantiles, as
  proposed by Gentleman and Crowley (1991). The \pkg{survival}::\code{\link[survival]{survfit}}
  conventions are followed for interpolation of these quantiles.
  Extreme values that are censored
  are drawn as open arrow heads rather than open circles.
  Left-censored values are shown as a shallow "V",
  which is actually just a rotated downward ">" sign. Similarly, right-censored
  values are shown as a deeper "^", which is a actually just a rotated upward ">" sign.
  Individual points are \code{\link{jitter}}ed, and open circles
  are used for complete observations
  to alleviate potential overlap and the danger of representing
  multiple points as a single point. Individual censored values are
  similarly jittered.
  With enough censored data
  observations in a group, certain quantiles may not be estimable, and
  thus a complete box would not appear.

  If \code{logscale=TRUE}, the tick marks for the y-axis
  on the left side of the plot show original values, while the
  ticks mark for the y-axis on the right side of the graph
  show base 10 log values.

  Tick marks are attempted to be chosen wisely. For log-scaled axes in
  particular, leading digits of 2, 5, and 10 for values are included if
  possible. Since the algorithm is empirical, the \code{ticklabels}
  argument is available for further refinement or complete replacement
  of tickmarks.

  The heading for the graph is taken from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{analysisname}
  argument.
  The label for the y-axis is taken from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{endptname}
  argument.
  The number of decimal places printed in the ticks on the y-axis is taken
  from the \code{\link{cgOneFactorData}} object,
  which \code{\link{prepareCGOneFactorData}} sets from its \code{digits}
  argument.

  The minimum and maximum values from the range of the data are
  respectively labeled in the bottom and top left corners of the graph
  region.

  If group labels along the x-axis seem to overlap in the standard
  horizontal form, they will be rotated 45 degrees.
}
\value{
  \code{boxplot.cgOneFactorData} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\references{
  Gentleman, R.C. and Crowley, J. (1991).  "Graphical Methods for
  Censored Data",
  \emph{Journal of the American Statistical Association}, Volume 86, 678-683.
}
\author{
  Bill Pikounis [aut, cre, cph], John Oleynick [aut], Eva Ye [ctb]
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
boxplot(canine.data)

## Plot the data on the original scale instead of the log scale
boxplot(canine.data, logscale=FALSE)

## Censored Data
data(gmcsfcens)
gmcsfcens.data <- prepareCGOneFactorData(gmcsfcens, format="groupcolumns",
                                         analysisname="cytokine",
                                         endptname="GM-CSF (pg/ml)",
                                         logscale=TRUE)
boxplot(gmcsfcens.data)
}
\concept{box plot}
