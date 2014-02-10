\name{errorBarGraph.cgOneFactorFit}
\docType{methods}
\alias{errorBarGraph,cgOneFactorFit-method}
\alias{errorBarGraph.cgOneFactorFit}
\title{
   Create an Error Bar graph amongst groups in a cgOneFactorFit object
}
\description{
  Creates a graph to see comparisons amongst groups based on the cgOneFactorFit
  object. The method of Andrews, Sarner, and Snee (1980) is applied to
  visualizes significant differences via non-overlapping error bars.
}
\usage{
\S4method{errorBarGraph}{cgOneFactorFit}(fit, mcadjust = FALSE, alpha =0.05,
 cgtheme = TRUE, device = "single", ...)  
}
\arguments{
  \item{fit }{
   A fit object of class \code{\link{cgOneFactorFit}}. 
 }
 \item{mcadjust }{
   Do a multiple comparisons adjustment, based on the simultaneous
   inference capabilities of the \pkg{multcomp} package. See Details
   below. The default value is \code{FALSE}.
   If \code{mcadjust=TRUE} is specified, there will be a delay,
   usually just for a few seconds, due to computing time of the
   critical point in order to conduct the adjusted comparisons.
 }
 \item{alpha }{Significance level, by default set to \code{0.05}, which
   equates to a 95\% confidence level.
 }
 \item{cgtheme }{
   When set to the default \code{TRUE}, ensures a trellis device is active with
   limited color scheme. Namely \code{background},
   \code{strip.shingle}, and \code{strip.background} are each set to \code{"white"}.
 }
 \item{device }{Can be one of three values:

   \describe{
     \item{\code{"single"}}{The default, which will put all graphs on the same
       device page. For example, when resistant & robust and classical
       least squares are present and \code{model="both"} (the default),
       a 2 x 1 paneled graph will be created.}
     \item{\code{"multiple"}}{Relevant only when resistant & robust and classical
       least squares are present and \code{model="both"} (the default). In
       that case, a new graphics device is generated to hold the resistant & robust
       version, as a single-paneled graph. 
       The classical least squares version is on the previous device.}
     \item{\code{"ask"}}{Relevant only when resistant & robust and classical
       least squares are present and \code{model="both"} (the default). In
       that case, each are portrayed as a single-paneled graph, with the
       \code{ask=TRUE} argument specified in \code{\link{par}} so that
       the user input confirmation is needed before the graphs are
       drawn.
       }
     }

   }
   \item{\dots }{
     Additional arguments. Two are currently valid: 
     \describe{
       \item{\code{model} }{For \code{cgOneFactorFit} objects that have 
	 classical least squares \code{lm()} or resistant & robust
	\code{rlm()} fits, the following argument values are possible:
	
	\describe{
	  \item{\code{"both"}}{Error Bar graphs based on both the ordinary 
	    classical least squares and resistant & robust fits are
	    performed. This is the default when both fits are present in
	    the \code{cgOneFactorFit} object specified in the \code{fit}
	    argument. If the resistant & robust fit is not available,
	    this value is not relevant.
	  }
	  \item{\code{"olsonly"}}{Only an Error Bar graph based on the ordinary 
	    classical least squares \code{olsfit} fit is performed.
	  }
	  \item{\code{"rronly"}}{Only a Error Bar Graph based on the resistant and
	    robust \code{rrfit} fit is performed.
	  }
	}
	
	For other possible \code{cgOneFactorFit} fit slots such as
	accelerated failure time or unequal variance models, the \code{model}
	argument is not relevant, and the appropriate comparisons table will
	be calculated for these model types.
      }

      \item{\code{ticklabels }}{A list of two components:
	\describe{
	  \item{\code{mod}}{Can be either of these two values,
	    \describe{
	      \item{\code{"replace"}}{Before graphing the data, remove
		any automatically generated tickmarks for the y-axis,
		and create the tickmarks specified in the
		\code{marks} component below.
	      }
	      \item{\code{"add"}}{Before graphing the data, add
		tickmarks specified in the 
		\code{marks} component below, to the automatically
		generated ones.
	      }
	    }
	  }
	  \item{\code{marks}}{A vector of tickmarks to be
	    placed on the y-axis. Any numeric representations will
    be coerced to character.	
	  }
	}
      }
    }
  }
}
\details{
  When \code{mcadjust=TRUE}, a status message of
  \code{"Some time may be needed as the critical point"}\cr
  \code{"from the multcomp::summary.glht function call is calculated"}
  is displayed at the console. This computed critical point
  is used for all interval calculations.

  The \pkg{multcomp} package provides a unified way to calculate
  critical points based on the comparisons of interest in a
  "family". Thus a user does not need to worry about choosing amongst
  the myriad names of multiple comparison procedures.

  The \code{errorBarGraph.cgOneFactorFit} method is only relevant for
  classical least squares and resistant & robust fits in the
  \code{\link{cgOneFactorFit}} object. There is an
  \code{\link{errorbargraph}} core function that could be used for
  approximations in other cases like accelerated failure time or unequal
  variance fits.

  The statistical method of Andrews, Sarner, and Snee (1980) is applied to
  visualize significant differences via non-overlapping error bars. The
  method is exact when there are equal sample sizes amongst the
  groups for the classical least squares case. When there are unequal
  group sample sizes or a resistant & robust fit is used to create the
  graph, the method is approximate, and this is noted in the main title
  section of the graph. For the unequal sample sizes, the harmonic mean
  is calculated to use for all the groups. The method's usefulness
  declines as the  sample sizes become more disparate.

  When two groups are compared, nonoverlapping error bars indicate a
  statistically significant pairwise difference. Conversely, if the
  error bars overlap, there is no such significant difference. In cases
  of approximation, or borderline overlap that is seen, the
  \code{\link{cgOneFactorComparisonsTable}} object created with
  \code{type="pairwisereflect"} or \code{type="pairwise"} needs to be
  consulted to judge significance with a p-value.

  The minimum and maximum values across all the bar ends
  are added inside the plot region in blue, flush against the
  y-axis. The number of decimal places are determined by the
  \code{digits} value in the \code{fit$settings} slot.
    
  If group labels along the x-axis seem to overlap in the standard
  horizontal form, they will be rotated 45 degrees.  
}
\value{
  \code{errorBarGraph.cgOneFactorFit} returns
  an invisible \code{NULL}. The main purpose is the side
  effect of graphing to the current device.
}
\references{
  Andrews, H.P., Snee, R.D., Sarner, M.H. (1980). "Graphical Display of Means,"
  \emph{The American Statistician}, 34, 195-199.

  Hothorn, T., Bretz, F., Westfall, P., Heiberger, R.M., and
  Schuetzenmeister, A. (2010). The \code{multcomp} R package.

  Hothorn, T., Bretz, F., and Westfall, P. (2008).
  "Simultaneous Inference in General Parametric Models",
  \emph{Biometrical Journal}, 50, 3, 346-363.
}
\author{
  Bill Pikounis, John Oleynick, Eva Ye
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

errorBarGraph(canine.fit)

errorBarGraph(canine.fit, mcadjust=TRUE, model="olsonly")
}
\concept{error bars}
\concept{comparison}
